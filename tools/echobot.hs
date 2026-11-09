{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
-- | A Tox bot on hs-toxcore and hs-toxav that sends friends back what they
-- send it: messages, and the audio and video of their calls. Clients can
-- test calls against it, also ones it places when asked with a `!` command.
-- rs-toxcore-c's echobot does the same on c-toxcore.
module Main (main) where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Exception       (AsyncException (..), SomeException,
                                          handleJust, try)
import           Control.Monad           (forM_, forever, void, when)
import qualified Crypto.Saltine.Class    as Sodium
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Base16
import qualified Data.ByteString.Char8   as Char8
import qualified Data.ByteString.Lazy    as LBS
import           Data.IORef              (IORef, atomicModifyIORef', newIORef,
                                          readIORef)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (mapMaybe)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Time               (UTCTime, defaultTimeLocale,
                                          diffUTCTime, formatTime,
                                          getCurrentTime)
import           Data.Word               (Word8)
import qualified Network.HTTP.Client     as Http
import qualified Network.HTTP.Client.TLS as Http
import           Options.Applicative
import           System.Directory        (doesFileExist, renameFile)
import           System.Environment      (lookupEnv)
import           System.FilePath         ((</>))
import           System.IO               (BufferMode (..), hSetBuffering,
                                          stdout)
import           Text.Read               (readMaybe)

import qualified Tox.Application.Tox     as Tox
import qualified Tox.AV                  as AV
import           Tox.AV                  (BitRates (..), Media (..))
import qualified Tox.Client.Debug        as Debug
import           Tox.Client.Nodes        (Node (..), bootstrapFrom, builtIn,
                                          nodesUrl, readNodes, usable)
import           Tox.Core.Log            (LogLevel (..))
import           Tox.Crypto.Core.Key     (PublicKey)
import qualified Tox.Session.Friend      as Messenger

data Args = Args
  { argSavefile      :: Maybe FilePath
  , argNick          :: String
  , argStatusMessage :: String
  , argNodesUrl      :: String
  , argNodes         :: [String]
  , argSnapshots     :: Maybe FilePath
  }

args :: Parser Args
args = Args
  <$> optional (strOption (long "savefile" <> short 's' <> metavar "FILE"))
  <*> strOption (long "nick" <> value "echobot-hs" <> showDefault)
  <*> strOption (long "status-message" <> value "Echoes messages and calls" <> showDefault)
  <*> strOption (long "nodes-url" <> value (Text.unpack nodesUrl) <> showDefault)
  <*> many (strOption (long "bootstrap-node" <> metavar "HOST:PORT:HEXPK"
        <> help "Bootstrap and TCP relay node, in place of the --nodes-url list. Repeatable."))
  <*> optional (strOption (long "snapshots" <> metavar "DIR"
        <> help "Where to keep each friend's latest picture as a greyscale PGM, every 10 seconds."))

-- | Kilobits per second, as rs-toxcore-c's echobot sends.
audioRate, videoRate :: Int
audioRate = 48
videoRate = 5000

-- | How long a friend waits between calls they ask for, and the longest they
-- can have one wait, say to lock their phone first.
callCooldown, maxCallDelay :: Int
callCooldown = 10
maxCallDelay = 120

saveInterval, statsInterval :: Int
saveInterval = 600
statsInterval = 10

usage :: BS.ByteString
usage = "!call [seconds]: I call you, after that many seconds if given (up to 120). \
        \!video [seconds]: with video. !hangup: I end our call. Anything else I send back."

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts <- execParser (info (args <**> helper) (progDesc "Echoes messages and calls, on hs-toxcore."))
  savefile <- case argSavefile opts of
    Just path -> pure path
    Nothing -> lookupEnv "BUILD_WORKSPACE_DIRECTORY" >>= maybe
      (fail "--savefile is needed outside `bazel run`")
      (pure . (</> "hs-tox/tools/echobot.tox"))
  saved <- doesFileExist savefile >>= \case
    True  -> Tox.ToxSave <$> BS.readFile savefile
    False -> pure Tox.NoSaveData
  tox <- Tox.new Tox.defaultOptions
    { Tox.optSaveData = saved
    , Tox.optLocalDiscovery = False
    , Tox.optLogger = \level line -> when (level >= LevelWarn) (putStrLn (Text.unpack line))
    } >>= either (fail . show) pure
  -- The arguments win over the save, so that renaming the bot renames it.
  void . Tox.selfSetName tox . Char8.pack $ argNick opts
  void . Tox.selfSetStatusMessage tox . Char8.pack $ argStatusMessage opts
  save tox savefile
  Tox.selfAddress tox >>= putStrLn . ("Bot ID: " <>) . Char8.unpack . Base16.encode
  nodes opts >>= mapM_ (bootstrapFrom tox) . take 8 . usable

  av <- AV.new AV.Transport
    { AV.sendLossless = Tox.friendQueueLosslessPacket tox
    , AV.sendLossy = Tox.friendQueueLossyPacket tox
    }
  asked <- newIORef Map.empty
  -- Each friend's latest picture, while they send video.
  videos <- newIORef Map.empty
  void . forkIO . forever $ AV.nextEvent av >>= \event -> do
    atomicModifyIORef' videos $ \showing -> (, ()) $ case event of
      AV.VideoReceived pk _ picture -> Map.insert pk picture showing
      AV.Ended pk _                 -> Map.delete pk showing
      _                             -> showing
    echoCall av event
  -- How each call's video comes, as hTox's debug line tells it.
  void . forkIO . forever $ do
    threadDelay (statsInterval * 1000000)
    now <- getCurrentTime
    readIORef videos >>= mapM_ (\(pk, picture) -> do
      AV.videoStats av pk >>= mapM_ (putStrLn . ((hex pk <> "'s ") <>) . Text.unpack . Debug.videoText)
      AV.videoEncoderName av pk >>= mapM_ (putStrLn . (("Sending " <> hex pk <> " video with ") <>))
      forM_ (argSnapshots opts) $ \dir ->
        snapshot (dir </> take 8 (hex pk) <> "-" <> formatTime defaultTimeLocale "%H%M%S" now <> ".pgm") picture)
      . Map.toList
  void . forkIO . forever $ threadDelay (saveInterval * 1000000) >> save tox savefile
  void . forkIO . forever $ Tox.nextEvent tox >>= handle tox av asked savefile
  -- Until Ctrl-C, which saves first.
  handleJust (\case UserInterrupt -> Just (); _ -> Nothing) pure . forever $ threadDelay 1000000000
  putStrLn "Shutting down"
  save tox savefile
  Tox.kill tox

-- | The picture's brightness, which shows what went wrong with one.
snapshot :: FilePath -> AV.Picture -> IO ()
snapshot path (AV.Picture w h y _ _) =
  BS.writeFile path (Char8.pack ("P5\n" <> show w <> " " <> show h <> "\n255\n") <> y)

-- | Written to a new file first, so that a crash can't leave half a save.
save :: Tox.Tox -> FilePath -> IO ()
save tox path = do
  bytes <- Tox.saveData tox
  BS.writeFile (path <> ".new") bytes
  renameFile (path <> ".new") path

-- | The nodes given, or else the list from the URL, or else the built-in
-- ones.
nodes :: Args -> IO [Node]
nodes opts
  | not (null (argNodes opts)) = pure (mapMaybe node (argNodes opts))
  | otherwise = do
      fetched <- try @SomeException $ do
        manager <- Http.newTlsManager
        request <- Http.parseRequest (argNodesUrl opts)
        Http.responseBody <$> Http.httpLbs request manager
      case either (const Nothing) (readNodes . LBS.toStrict) fetched of
        Just list -> pure list
        Nothing   -> putStrLn "Using the built-in nodes" >> pure builtIn
  where
    node spec = case Text.splitOn ":" (Text.pack spec) of
      [host, port, key] | Just p <- readMaybe (Text.unpack port), Right k <- Base16.decode (Text.encodeUtf8 key) ->
        Just (Node (Text.unpack host) p (Just p) k True)
      _ -> Nothing

-- | Accepts friend requests, echoes messages, does `!` commands, and passes
-- calls' packets to ToxAV.
handle :: Tox.Tox -> AV.ToxAV PublicKey -> IORef (Map.Map PublicKey UTCTime) -> FilePath -> Tox.Event -> IO ()
handle tox av asked savefile = \case
  Tox.FriendRequest pk _ -> do
    putStrLn $ "Accepting " <> hex pk <> "'s friend request"
    Tox.friendAddNoRequest tox pk >>= either (putStrLn . ("Failed to accept: " <>) . show) (const (save tox savefile))
  Tox.FriendMessage pk ty message -> case Char8.uncons (Char8.dropWhile (== ' ') message) of
    Just ('!', request) -> do
      reply <- run av asked pk (Char8.unpack request)
      putStrLn $ hex pk <> " sent " <> show message <> ": " <> Char8.unpack reply
      void $ Tox.friendSendMessage tox pk Messenger.MessageNormal reply
    _ -> void $ Tox.friendSendMessage tox pk ty message
  Tox.FriendConnectionChanged pk status -> AV.friendConnection av pk (status /= Tox.NotConnected)
  Tox.FriendLosslessPacket pk packet -> forward AV.losslessIds pk packet
  Tox.FriendLossyPacket pk packet -> forward AV.lossyIds pk packet
  Tox.SelfConnectionChanged status -> putStrLn $ "Connection: " <> show status
  _ -> pure ()
  where
    forward :: [Word8] -> PublicKey -> BS.ByteString -> IO ()
    forward ids pk packet = forM_ (BS.uncons packet) $ \(ident, _) ->
      when (ident `elem` ids) $ AV.receivePacket av pk packet

-- | The reply to a `!` command.
run :: AV.ToxAV PublicKey -> IORef (Map.Map PublicKey UTCTime) -> PublicKey -> String -> IO BS.ByteString
run av asked pk request = case words request of
  [name] | name `elem` ["call", "video"] -> calling name 0
  [name, delay] | name `elem` ["call", "video"], Just seconds <- readMaybe delay
                , seconds >= 0, seconds <= maxCallDelay -> calling name seconds
  ["hangup"] -> AV.control av pk AV.Cancel >>= pure . either (const "We're not in a call.") (const "Hung up.")
  _ -> pure usage
  where
    calling name seconds = do
      now <- getCurrentTime
      allowed <- atomicModifyIORef' asked $ \m -> case Map.lookup pk m of
        Just at | diffUTCTime now at < fromIntegral callCooldown -> (m, False)
        _ -> (Map.insert pk now m, True)
      if not allowed then pure "Not so fast: wait a few seconds between calls." else do
        void . forkIO $ do
          threadDelay (seconds * 1000000)
          let video = if name == "video" then videoRate else 0
          AV.call av pk (BitRates (fromIntegral audioRate) (fromIntegral video))
            >>= either (putStrLn . ("Failed to call: " <>) . show) (const (putStrLn ("Calling " <> hex pk)))
        pure $ if seconds == 0 then "Calling you." else "Calling you in " <> Char8.pack (show seconds) <> " seconds."

-- | Answers calls and sends back what comes, with the times it was captured
-- at, so that its audio and video stay in step as the friend sent them.
echoCall :: AV.ToxAV PublicKey -> AV.Event PublicKey -> IO ()
echoCall av = \case
  AV.Invited pk caps -> do
    putStrLn $ "Answering " <> hex pk <> "'s call (video: " <> show (AV.sendsVideo caps) <> ")"
    AV.answer av pk (rates caps) >>= either (putStrLn . ("Failed to answer: " <>) . show) pure
  AV.PeerChanged pk caps ->
    -- Video back only while the friend sends some.
    void $ AV.setBitRate av pk Video (if AV.sendsVideo caps then videoRate else 0)
  AV.AudioReceived pk _ captured frame -> void $ AV.sendAudio av pk captured frame
  AV.VideoReceived pk captured picture -> void $ AV.sendVideo av pk captured picture
  AV.Ended pk end -> putStrLn $ hex pk <> "'s call ended: " <> show end
  where
    rates caps = BitRates (fromIntegral audioRate) (if AV.sendsVideo caps then fromIntegral videoRate else 0)

hex :: PublicKey -> String
hex = Char8.unpack . Base16.encode . Sodium.encode
