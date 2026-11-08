{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | How events change a client, and the commands for Tox.
module Tox.Client.Update
  ( Event (..)
  , Refusal (..)
  , Chat (..)
  , Viewing (..)
  , Command (..)
  , update
  , drafted
  , memberByKey
  , quoted
  , forwardable
  , interrupted
  ) where

import           Control.Applicative    ((<|>))
import           Control.Monad          (mfilter)
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.List              (find, nub)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe, isJust, isNothing,
                                         listToMaybe, mapMaybe)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Data.Time              (UTCTime, diffUTCTime)
import           Data.Word              (Word64)
import qualified Tox.Application.Tox    as Tox
import qualified Tox.Sync.Account       as Sync
import qualified Tox.Sync.Event         as Sync
import qualified Tox.Sync.Fact          as Sync
import qualified Tox.Sync.Id            as Sync
import qualified Tox.Sync.Protocol      as Protocol
import qualified Tox.Sync.Replica       as Replica
import qualified Tox.Sync.Setting       as Sync
import qualified Tox.Sync.Store         as Sync

import           Tox.Client
import qualified Tox.Client.Debug       as Debug
import           Tox.Client.Formats     (Format (..), Reading (..), formats,
                                         readWith)
import qualified Tox.Client.Linked      as Linked
import           Tox.Client.Linked      (Linked)
import           Tox.Client.Locations   (locationText)
import           Tox.Client.Messages    (parts)
import qualified Tox.Client.Pictures    as Pictures
import           Tox.Client.Types

-- | What Tox reports, and what the user wants done.
data Event
  = Started Text
    -- ^ Tox is running, with this Tox ID.
  | Connected Connection
  | FriendOnline ContactId Bool
  | FriendNamed ContactId Text
  | FriendStatusMessage ContactId Text
  | FriendStatus ContactId Tox.UserStatus
  | FriendAvatar ContactId (Maybe BS.ByteString)
    -- ^ PNG, or Nothing if they removed theirs.
  | Requested ContactId Text
  | AddFailed Text (Maybe Tox.AddFriendError)
    -- ^ Tox refused to add a Tox ID, Nothing if it isn't one.
  | Received ContactId UTCTime Tox.MessageType BS.ByteString
  | Delivering ContactId MessageId Delivery
  | Confirmed ContactId MessageId Int UTCTime
    -- ^ How many parts of our message the friend has.
  | FileStarted ContactId MessageId FileRef
    -- ^ Tox offered the friend our file.
  | FileOffered ContactId FileRef UTCTime Text Word64 FilePath BS.ByteString
    -- ^ Name, size, path to save it to, and file ID.
  | FileMoved ContactId FileRef Word64
    -- ^ Bytes so far.
  | FileEnded ContactId FileRef Bool
    -- ^ Whether the whole file got through.
  | CallInvited ContactId UTCTime Bool
    -- ^ With video or not.
  | CallPeer ContactId UTCTime Bool
    -- ^ The friend answered our call, or turned video on or off.
  | CallEnded ContactId UTCTime
  | CallGlared ContactId UTCTime
    -- ^ Tox failed the call because both ends called at once.
  | CallElsewhere UTCTime
    -- ^ Another profile has a call (we only do 1 at a time).
  | Tick UTCTime
    -- ^ Each second during calls and on the debug screen.
  | Invited ContactId GroupId GroupKind Text BS.ByteString
    -- ^ Group name and invite data.
  | GroupCreated GroupId GroupKind Text UTCTime
    -- ^ We founded it.
  | GroupConnected GroupId UTCTime
    -- ^ We have the group state from peers.
  | GroupRefused GroupId Tox.JoinFail
  | MembersListed GroupId UTCTime [(Tox.PeerId, Member)]
    -- ^ Everyone in a conference now, except us.
  | MemberJoined GroupId UTCTime Tox.PeerId Member
  | MemberLeft GroupId UTCTime Tox.PeerId Tox.ExitType
  | MemberNamed GroupId Tox.PeerId Text
  | MemberStatus GroupId Tox.PeerId Tox.UserStatus
  | RolesKnown GroupId (Maybe Tox.Role) [(Tox.PeerId, Tox.Role)]
    -- ^ Ours and the members'.
  | SettingsKnown GroupId Tox.GroupSettings
  | GroupReceived GroupId UTCTime Tox.PeerId Tox.MessageType Text
  | PrivateReceived GroupId UTCTime Tox.PeerId Tox.MessageType Text
  | GroupSent GroupId MessageId
  | GroupSaved GroupId BS.ByteString
  | GroupNamed GroupId Text
  | TopicChanged GroupId UTCTime (Maybe Tox.PeerId) Text
    -- ^ Peer, if Tox knows who.
  | TopicSetByUs GroupId UTCTime Text
  | TopicAllowed GroupId Bool
  | GroupModerated GroupId UTCTime (Maybe Tox.PeerId) (Maybe Tox.PeerId) Tox.Moderation
    -- ^ Who did it to whom. Nothing means us.
  | SyncReceived ContactId BS.ByteString
    -- ^ Only a linked account's home sends these.
  | FileShared ContactId UTCTime Text Word64 BS.ByteString FilePath
    -- ^ Our file for a linked account's friend, stored as a blob for the
    -- home: name, size, blob ID and path.
  | BlobArrived BS.ByteString FilePath (Maybe BS.ByteString)
    -- ^ From a linked account's home, saved at the path. Bytes too if it's
    -- small enough to be an avatar.
  | BlobMissing BS.ByteString
    -- ^ Came out of order, cut short, or doesn't match its ID.
  | EarlierMessages ContactId [Message]
    -- ^ From the store, those before the oldest we have. None if there are
    -- no more.
  | EarlierEntries GroupId [GroupEntry]
  | CloseChat Chat
    -- ^ The user left it, so it keeps only its newest.
  | ToxRefused Refusal
  | InviteSent GroupId ContactId
  | Measured Debug.Report
    -- ^ As asked with 'MeasureDebug'.
  | ChangeProfile Text Text
    -- ^ Nickname and status message.
  | SetAway Tox.UserStatus
  | SetAvatar BS.ByteString
    -- ^ Our new avatar as a PNG.
  | RemoveAvatar
  | SetAutoAccept Word64
  | ToggleFormat Text
  | SetDevice DeviceSettings
  | AddFriend Text Text
    -- ^ Tox ID and message.
  | AcceptRequest ContactId
  | RejectRequest ContactId
  | DeleteContact ContactId Bool
    -- ^ Keep the chat or not.
  | MuteChat ContactId (Maybe UTCTime)
    -- ^ Until a time, or forever.
  | UnmuteChat ContactId
  | SendPictures ContactId (Maybe Text)
    -- ^ Picture format to send them, by its key.
  | MarkRead Chat
  | SetDraft Chat Text
  | Stage ContactId Staged
  | Unstage ContactId Int
    -- ^ By its index in the staged list.
  | ReplyTo ContactId MessageId
  | CancelReply ContactId
  | Send Chat UTCTime
    -- ^ Its draft, with what's staged.
  | SendPrivately GroupId Text UTCTime
    -- ^ The group's draft, to the member with this key.
  | Forward ContactId [MessageId] ContactId UTCTime
  | Download ContactId MessageId
  | Cancel ContactId MessageId
  | StartCall ContactId UTCTime Bool
    -- ^ With or without the camera.
  | AnswerCall UTCTime Bool
  | HangUp UTCTime
  | ToggleMute
  | ToggleCamera
  | AnswerInvite ContactId GroupId (Maybe Text)
    -- ^ Join with this password, or decline.
  | Found Founding Text
    -- ^ A new group with this name.
  | JoinPublic GroupId Text
    -- ^ With its password.
  | InviteFriend GroupId ContactId
  | LeaveGroup GroupId
  | DeleteGroup GroupId
  | MuteGroup GroupId (Maybe UTCTime)
  | UnmuteGroup GroupId
  | ToggleMentions GroupId
  | SetTopic GroupId Text
  | Moderate GroupId Tox.PeerId MemberAction
  | ChangeSettings GroupId Tox.GroupSettings
  | ChooseRelayBot GroupId (Maybe Text)
    -- ^ Member key of the relay bot, or none.
  deriving (Eq, Show)

data Chat = FriendChat ContactId | GroupChat GroupId
  deriving (Eq, Show)

-- | What Tox didn't do.
data Refusal
  = TopicRefused GroupId
  | FoundingRefused Founding
  | InviteRefused GroupId ContactId
  | ModerationRefused GroupId Tox.PeerId Tox.ModError
  | SettingRefused GroupId
  deriving (Eq, Show)

-- | What the app shows, for unread counts and read marks, and how much of
-- each chat it keeps in memory.
data Viewing = Viewing
  { viewingChat  :: Maybe Chat
    -- ^ Open in the app. It keeps all it has (the user may be reading back).
  , viewingShown :: Bool
    -- ^ The app is in front.
  , viewingEnd   :: Bool
    -- ^ Scrolled to its newest messages.
  , viewingPage  :: Int
    -- ^ Other chats keep this many of their newest, and those still in use.
  } deriving (Eq, Show)

update :: Viewing -> Event -> Client -> (Client, [Command])
update viewing event client = case clientLinked client of
  Just l  -> linkedUpdate l viewing event client
  Nothing -> ownUpdate viewing event client

-- | Whether the chat has a draft or staged items to send.
drafted :: Chat -> Client -> Bool
drafted chat client = case chat of
  FriendChat cid -> written (Map.lookup cid (clientDrafts client)) || not (null (Map.findWithDefault [] cid (clientStaged client)))
  GroupChat gid  -> written (Map.lookup gid (clientGroupDrafts client))
  where written = maybe False (not . Text.null . Text.strip)

readable :: AccountSettings -> [Format]
readable account = [f | f <- formats, formatKey f `Set.notMember` settingFormatsOff account]

forwardable :: Message -> Bool
forwardable m = isNothing (messageCall m)
  && maybe True (\a -> messageMine m || attachmentState a == Complete) (messageFile m)

quoted :: Message -> Text
quoted = Text.intercalate "\n" . map ("> " <>) . Text.lines . messageText

-- | For Tox to do.
data Command
  = SetProfile Profile
  | SetGroupStatus GroupId Tox.UserStatus
  | Befriend ContactId
    -- ^ Without a request: to accept theirs, or to add a friend back.
  | Unfriend ContactId
  | SendAvatar ContactId (Maybe BS.ByteString)
    -- ^ Our avatar as a PNG, or Nothing if we have none.
  | RequestFriend Text Text
    -- ^ Tox ID and message.
  | SendMessage ContactId MessageId Text Int
    -- ^ With how many of its parts the friend confirmed.
  | SendFile ContactId MessageId FilePath Text Word64 (Maybe BS.ByteString)
  | AcceptFile ContactId FileRef FilePath
    -- ^ Resumes after what's already in the file.
  | CancelFile ContactId FileRef
  | ForgetFile FilePath
    -- ^ Delete our copy of a file that was unstaged.
  | CallFriend ContactId Bool
    -- ^ With or without the camera, as for the other call commands.
  | AnswerFriend ContactId Bool
  | HangUpCall ContactId
  | CameraCall ContactId Bool
  | JoinGroup ContactId GroupId BS.ByteString Text Text
    -- ^ With the friend's invite, our name in the group and a password.
  | JoinConference ContactId GroupId BS.ByteString
  | RestoreGroup GroupKind GroupId BS.ByteString
  | SendGroupMessage GroupId MessageId Text
  | QuitGroup GroupId
  | SetGroupTopic GroupId Text
  | FoundGroup Founding Text Text
    -- ^ Group name and our name in it.
  | JoinPublicGroup GroupId Text Text
    -- ^ Our name in it and its password.
  | InviteToGroup GroupId ContactId
  | SendPrivate GroupId MessageId Tox.PeerId Text
  | ModerateMember GroupId Tox.PeerId MemberAction
  | SetGroupSettings GroupId Tox.GroupSettings
  | SendSync ContactId [BS.ByteString]
    -- ^ For a linked account's home.
  | FetchBlob ContactId BS.ByteString [BS.ByteString] (Maybe Text)
    -- ^ Get a blob from the home with these packets, unless we have it
    -- already (a file under its name, an avatar with the blobs).
  | ServeBlob ContactId BS.ByteString [BS.ByteString]
    -- ^ Send the home a blob it asked for, or these packets if we don't
    -- have it.
  | StoreBlob BS.ByteString
    -- ^ So the home can ask for it.
  | ShareFile ContactId FilePath Text Word64
    -- ^ Store a file as a blob, for the home to send to a linked account's
    -- friend.
  | MeasureDebug
  deriving (Eq, Show)


-- | For profiles whose Tox runs on this device.
ownUpdate :: Viewing -> Event -> Client -> (Client, [Command])
ownUpdate viewing event client = case event of
  -- Only a linked account's home sends these.
  SyncReceived _ _ -> only client
  BlobArrived {} -> only client
  BlobMissing {} -> only client
  FileShared {} -> only client
  -- For the user to see.
  ToxRefused _ -> only client
  InviteSent {} -> only client
  Measured _ -> only client
  SetAvatar png ->
    let hash = SHA256.hash png
    in ( pruned client { clientAvatar = Just hash, clientAvatars = Map.insert hash png (clientAvatars client) }
       , [SendAvatar cid (Just png) | (cid, c) <- Map.toList (clientContacts client), contactOnline c] )
  RemoveAvatar ->
    ( pruned client { clientAvatar = Nothing }
    , [SendAvatar cid Nothing | (cid, c) <- Map.toList (clientContacts client), contactOnline c] )
  FriendAvatar cid png -> only $ pruned $
    (adjusted cid (\c -> c { contactAvatar = SHA256.hash <$> png }))
      { clientAvatars = maybe id (\p -> Map.insert (SHA256.hash p) p) png (clientAvatars client) }
  SetAutoAccept size -> only client { clientAccount = (clientAccount client) { settingAutoAccept = size } }
  -- Read their messages again (as Tox carried them if the format is now
  -- off).
  ToggleFormat key ->
    let account = clientAccount client
        off = settingFormatsOff account
        account' = account { settingFormatsOff = (if Set.member key off then Set.delete key else Set.insert key) off }
        reread m = case messageRaw m of
          Just raw | not (messageMine m) -> m { messageText = readText (readWith (readable account') raw) }
          _ -> m
    in only client
         { clientAccount = account'
         , clientContacts = (\c -> c { contactMessages = map reread (contactMessages c) }) <$> clientContacts client
         , clientFormer = (\c -> c { contactMessages = map reread (contactMessages c) }) <$> clientFormer client }
  SetDevice device -> only client { clientDevice = device }
  SetDraft (FriendChat cid) draft -> only client { clientDrafts = Map.insert cid draft (clientDrafts client) }
  SetDraft (GroupChat gid) draft -> only client { clientGroupDrafts = Map.insert gid draft (clientGroupDrafts client) }
  MarkRead (FriendChat cid) -> only $ adjusted cid (\c -> c { contactUnread = 0 })
  MarkRead (GroupChat gid) -> only $ adjustedGroup gid (\g -> g { groupUnread = 0 })
  EarlierMessages cid page -> only $ adjusted cid $ \c ->
    c { contactMessages = inFront messageId page (contactMessages c), contactEarlier = not (null page) }
  EarlierEntries gid page -> only $ adjustedGroup gid $ \g ->
    g { groupEntries = inFront entryId page (groupEntries g), groupEarlier = not (null page) }
  CloseChat (FriendChat cid) -> only $ adjusted cid (trimmed cid)
  CloseChat (GroupChat gid) -> only $ adjustedGroup gid trimmedGroup
  Send (GroupChat gid) time -> groupSend gid Nothing time
  SendPrivately gid key time -> groupSend gid (Just key) time
  -- Staged items first, then the text.
  Send (FriendChat cid) time
    | Just contact <- Map.lookup cid (clientContacts client)
    , let staged = Map.findWithDefault [] cid (clientStaged client)
          draft = maybe "" Text.strip (Map.lookup cid (clientDrafts client))
    , not (null staged) || not (Text.null draft) ->
        let replied = Map.lookup cid (clientReplies client) >>= \i -> find ((== i) . messageId) (contactMessages contact)
            message = maybe id (\m t -> quoted m <> "\n" <> t) replied draft
            picked = \case
              StagedFile path name size fileId -> ours name time (Just (Attachment size path 0 Waiting (Just fileId)))
              StagedPlace latitude longitude   -> ours (locationText latitude longitude) time Nothing
            sent = zipWith ($) (map picked staged ++ [ours message time Nothing | not (Text.null draft)]) newIds
        in
        ( using (length sent) $ (withContact cid (grown cid contact { contactMessages = contactMessages contact ++ sent }))
            { clientDrafts = Map.delete cid (clientDrafts client)
            , clientStaged = Map.delete cid (clientStaged client)
            , clientReplies = if Text.null draft then clientReplies client else Map.delete cid (clientReplies client) }
        , [ case messageFile m of
              Just a  -> SendFile cid (messageId m) (attachmentPath a) (messageText m) (attachmentSize a) (attachmentId a)
              Nothing -> SendMessage cid (messageId m) (messageText m) 0
          | contactOnline contact, m <- sent ]
        )
    | otherwise -> only client
  ChangeProfile nickname status ->
    let applied = (clientProfile client) { profileNickname = Text.strip nickname, profileStatus = Text.strip status }
    in (client { clientProfile = applied }, [SetProfile applied])
  SetAway away ->
    let chosen = (clientProfile client) { profileAway = away } in (client { clientProfile = chosen }, [SetProfile chosen])
  AddFriend address message ->
    let cid = ContactId (Text.toUpper (Text.take 64 address))
        request = (Text.toUpper address, Text.strip message)
        requesting c = c { contactRequest = Just request }
    in ( client
           { clientContacts = Map.alter (Just . requesting . fromMaybe (known cid)) cid (clientContacts client)
           , clientFormer = Map.delete cid (clientFormer client)
           , clientRequests = Map.delete cid (clientRequests client)
           , clientDeclined = Map.delete cid (clientDeclined client)
           }
       , [uncurry RequestFriend request]
       )
  AddFailed address _ ->
    let cid = ContactId (Text.toUpper (Text.take 64 address))
        forget c
          | null (contactMessages c) = Nothing
          | otherwise = Just c { contactRequest = Nothing }
    in only client { clientContacts = Map.update forget cid (clientContacts client) }
  AcceptRequest cid
    | Map.member cid (clientRequests client) ->
        ( (withContact cid (known cid))
            { clientRequests = Map.delete cid (clientRequests client)
            , clientFormer = Map.delete cid (clientFormer client)
            , clientDeclined = Map.delete cid (clientDeclined client) }
        , [Befriend cid]
        )
    | otherwise -> only client
  RejectRequest cid -> only client
    { clientRequests = Map.delete cid (clientRequests client)
    , clientDeclined = maybe id (Map.insert cid) (Map.lookup cid (clientRequests client)) (clientDeclined client)
    }
  Started address ->
    ( client { clientProfile = (clientProfile client) { profileToxId = address }
             -- Tox only knows these again once it hears from peers.
             , clientGroups = (\g -> g { groupPhase = if groupPhase g == InGroup then Joining else groupPhase g
                                       , groupMembers = Map.empty, groupSince = Nothing, groupRoles = Map.empty
                                       , groupSelfRole = Nothing, groupSettings = Nothing }) <$> clientGroups client }
    , SetProfile (clientProfile client)
        : [ maybe (Befriend cid) (uncurry RequestFriend) (contactRequest c)
          | (cid, c) <- Map.toList (clientContacts client) ]
        -- After the friends (they may help find the peers).
        ++ [ RestoreGroup (groupKind g) gid saved
           | (gid, g) <- Map.toList (clientGroups client), groupPhase g `elem` [Joining, InGroup]
           , Just saved <- [groupSaved g] ]
    )
  Connected connection -> only client { clientConnection = connection }
  FriendOnline cid online -> case Map.lookup cid (clientContacts client) of
    Just contact ->
      -- Tox ends a friend's transfers when they go offline. We offer ours
      -- again when they're back.
      let (ended, going) = Map.partitionWithKey (\(c, _) _ -> c == cid && not online) (clientTransfers client)
          cut = foldr (\i -> adjustMessage cid i interrupted) client { clientTransfers = going } (Map.elems ended)
          -- Tox also says online when they move between TCP and UDP.
          returned = online && not (contactOnline contact)
          met c = c { contactOnline = online, contactRequest = if online then Nothing else contactRequest c
                    , contactStatus = if online then contactStatus c else Tox.Online }
          -- Drop invites we answered, now the friend is back to get the answer.
          spent (c, _) invite = c /= cid || not returned || isNothing (inviteAnswered invite)
      in ( cut { clientContacts = Map.adjust met cid (clientContacts cut)
               , clientInvites = Map.filterWithKey spent (clientInvites cut) }
         , [ SendMessage cid (messageId m) (messageText m) (messageParts m)
           | returned
           , m <- contactMessages contact
           , messageMine m, messageDelivery m /= Delivered, Nothing <- [messageFile m] ]
           ++
           [ SendAvatar cid (Just png) | returned, Just png <- [(`Map.lookup` clientAvatars client) =<< clientAvatar client] ]
           ++
           [ SendFile cid (messageId m) (attachmentPath a) (messageText m) (attachmentSize a) (attachmentId a)
           | returned
           , m <- contactMessages contact
           , messageMine m, Just a <- [messageFile m], attachmentState a == Waiting ]
           -- Invites we accepted while the friend was away (Tox can only
           -- answer them through the friend).
           ++
           [ joinCommand cid gid invite password
           | returned
           , ((c, gid), invite) <- Map.toList (clientInvites client), c == cid
           , Just password <- [inviteAnswered invite] ]
         )
    Nothing -> only client
  FriendNamed cid name -> only $ adjusted cid (\c -> c { contactName = Just name })
  FriendStatusMessage cid status -> only $ adjusted cid (\c -> c { contactStatusMessage = status })
  FriendStatus cid status -> only $ adjusted cid (\c -> c { contactStatus = status })
  DeleteContact cid keep -> case Map.lookup cid (clientContacts client) of
    Just contact ->
      let calling = [c | Just c <- [clientCall client], callContact c == cid]
      in ( client
             { clientContacts = Map.delete cid (clientContacts client)
             , clientFormer = if keep
                 then Map.insert cid contact { contactOnline = False, contactUnread = 0, contactRequest = Nothing }
                        (clientFormer client)
                 else clientFormer client
             , clientDrafts = Map.delete cid (clientDrafts client)
             , clientStaged = Map.delete cid (clientStaged client)
             , clientInvites = Map.filterWithKey (\(c, _) _ -> c /= cid) (clientInvites client)
             , clientTransfers = Map.filterWithKey (\(c, _) _ -> c /= cid) (clientTransfers client)
             , clientCall = if null calling then clientCall client else Nothing
             }
         , [HangUpCall cid | not (null calling)] ++ [Unfriend cid]
           ++ [ForgetFile path | StagedFile path _ _ _ <- Map.findWithDefault [] cid (clientStaged client)] )
    Nothing -> only client
  MuteChat cid until' -> only $ adjusted cid (\c -> c { contactMute = Just (maybe MutedAlways MutedUntil until') })
  UnmuteChat cid -> only $ adjusted cid (\c -> c { contactMute = Nothing })
  Received cid time kind bytes
    -- Drop a message with the same key as an earlier one (some clients resend
    -- messages).
    | Just key <- readRepeat got
    , Just c <- Map.lookup cid (clientContacts client)
    , key `elem` [ k | m <- contactMessages c, not (messageMine m), Just raw <- [messageRaw m]
                     , Just k <- [readRepeat (readWith (readable (clientAccount client)) raw)] ] -> only client
    | otherwise -> only $ using 1 $ adjusted cid $ \c -> grown cid c
        { contactMessages = contactMessages c ++ [Message False (readText got) time Delivered Nothing Nothing 0 Nothing kind (Just bytes) newId]
        , contactUnread = contactUnread c + (if reading (FriendChat cid) then 0 else 1)
        }
    where got = readWith (readable (clientAccount client)) bytes
  Delivering cid i delivery -> only $ adjustMessage cid i (\m ->
    if messageDelivery m /= Delivered then m { messageDelivery = delivery } else m) client
  Confirmed cid i n time -> only $ adjustMessage cid i (\m ->
    let confirmed = max n (messageParts m)
        whole = confirmed >= length (parts (messageText m))
    in m { messageParts = confirmed
         , messageDelivery = if whole then Delivered else messageDelivery m
         , messageDelivered = if whole && isNothing (messageDelivered m) then Just time else messageDelivered m
         }) client
  ReplyTo cid i
    | Just m <- messageOf cid i, isNothing (messageCall m) -> only client { clientReplies = Map.insert cid i (clientReplies client) }
    | otherwise -> only client
  CancelReply cid -> only client { clientReplies = Map.delete cid (clientReplies client) }
  Forward source is target time -> case Map.lookup target (clientContacts client) of
    Just contact ->
      -- In the order of their chat.
      let sent = [ m | m <- maybe [] contactMessages (Map.lookup source (clientContacts client)), messageId m `elem` is, forwardable m ]
          copies = zipWith copy sent newIds
          copy m = ours (messageText m) time (forwardedFile <$> messageFile m)
          forwardedFile a = a { attachmentState = Waiting, attachmentDone = 0, attachmentId = Nothing }
      in ( using (length copies) (withContact target (grown target contact { contactMessages = contactMessages contact ++ copies }))
         , [ case messageFile m of
               Nothing -> SendMessage target (messageId m) (messageText m) 0
               Just a  -> SendFile target (messageId m) (attachmentPath a) (messageText m) (attachmentSize a) (attachmentId a)
           | contactOnline contact, m <- copies ] )
    Nothing -> only client
  Requested cid message
    | Map.member cid (clientContacts client) -> (client, [Befriend cid])
    | Map.lookup cid (clientDeclined client) == Just message -> only client
    | otherwise -> only client { clientRequests = Map.insert cid message (clientRequests client) }
  Stage cid item -> only client { clientStaged = Map.insertWith (flip (++)) cid [item] (clientStaged client) }
  Unstage cid k ->
    let staged = Map.findWithDefault [] cid (clientStaged client)
    in ( client { clientStaged = Map.filter (not . null) (Map.insert cid (take k staged ++ drop (k + 1) staged) (clientStaged client)) }
       , [ForgetFile path | StagedFile path _ _ _ <- take 1 (drop k staged)] )
  FileStarted cid i ref -> only client { clientTransfers = Map.insert (cid, ref) i (clientTransfers client) }
  FileOffered cid ref time name size path fileId -> case Map.lookup cid (clientContacts client) of
    -- Resume a file of theirs that was cut off, in the same message.
    Just contact
      | (i, a) : _ <- [ (messageId m, a) | m <- contactMessages contact, not (messageMine m)
                               , Just a <- [messageFile m], attachmentId a == Just fileId
                               , attachmentSize a == size, attachmentState a `elem` [Waiting, Interrupted] ] ->
          let taken = attachmentState a == Interrupted || size <= settingAutoAccept (clientAccount client)
          in ( (adjustFile cid i (\b -> b { attachmentState = if taken then Moving else Waiting }) client)
                 { clientTransfers = Map.insert (cid, ref) i (clientTransfers client) }
             , [AcceptFile cid ref (attachmentPath a) | taken] )
    Just contact ->
      let taken = size <= settingAutoAccept (clientAccount client)
          file = Attachment size path 0 (if taken then Moving else Waiting) (Just fileId)
      in ( (using 1 $ withContact cid $ grown cid contact
             { contactMessages = contactMessages contact ++ [Message False name time Delivered (Just file) Nothing 0 Nothing Tox.MessageNormal Nothing newId]
             , contactUnread = contactUnread contact + (if reading (FriendChat cid) then 0 else 1)
             }) { clientTransfers = Map.insert (cid, ref) newId (clientTransfers client) }
         , [AcceptFile cid ref path | taken]
         )
    Nothing -> (client, [CancelFile cid ref])
  Download cid i
    | Just ref <- liveRef cid i ->
        ( adjustFile cid i (\a -> a { attachmentState = Moving }) client
        , [AcceptFile cid ref (attachmentPath a) | Just a <- [fileOf cid i]] )
    | otherwise -> only client
  Cancel cid i ->
    ( adjustFile cid i (\a -> a { attachmentState = Cancelled })
        client { clientTransfers = maybe id (\ref -> Map.delete (cid, ref)) (liveRef cid i) (clientTransfers client) }
    , [CancelFile cid ref | Just ref <- [liveRef cid i]] )
  FileMoved cid ref done -> only $ case Map.lookup (cid, ref) (clientTransfers client) of
    Just i  -> adjustFile cid i (\a -> a { attachmentDone = done, attachmentState = Moving }) client
    Nothing -> client
  FileEnded cid ref whole -> only $ case Map.lookup (cid, ref) (clientTransfers client) of
    Just i -> adjustFile cid i
      (\a -> if whole then a { attachmentDone = attachmentSize a, attachmentState = Complete }
             else a { attachmentState = Cancelled })
      client { clientTransfers = Map.delete (cid, ref) (clientTransfers client) }
    Nothing -> client
  StartCall cid time camera
    | Nothing <- clientCall client
    , Just contact <- Map.lookup cid (clientContacts client)
    , contactOnline contact ->
        (begun (newCall cid Calling time camera) { callCamera = camera }, [CallFriend cid camera])
    | otherwise -> only client
  AnswerCall time camera -> case clientCall client of
    Just c | callPhase c == Ringing ->
      ( client { clientCall = Just c { callPhase = Talking time, callCamera = camera } }
      , [AnswerFriend (callContact c) camera] )
    _ -> only client
  HangUp time -> case clientCall client of
    Just c  -> ((recorded time False c) { clientCall = Nothing }, [HangUpCall (callContact c)])
    Nothing -> only client
  -- Their call counts as missed, and we don't place ours.
  CallElsewhere time -> case clientCall client of
    Just c  -> ((recorded time (callPhase c == Ringing) c) { clientCall = Nothing }, [HangUpCall (callContact c)])
    Nothing -> only client
  ToggleMute -> only client { clientCall = (\c -> c { callMuted = not (callMuted c) }) <$> clientCall client }
  -- Tox only lets us change this once the call is going.
  ToggleCamera -> case clientCall client of
    Just c | Talking _ <- callPhase c ->
      let camera = not (callCamera c)
      in (client { clientCall = Just c { callCamera = camera } }, [CameraCall (callContact c) camera])
    _ -> only client
  -- Their call came in before ours went out, or it's the call again after
  -- a glare. Either way, answer it.
  CallInvited cid time video -> case clientCall client of
    Just c | callContact c == cid, callPhase c == Calling, callGlare c /= Settling ->
      ( client { clientCall = Just c { callPhase = Talking time, callPeerVideo = video, callGlare = Settling } }
      , [AnswerFriend cid (callCamera c)] )
    Nothing | Map.member cid (clientContacts client) ->
      only (begun (newCall cid Ringing time video) { callPeerVideo = video })
    _ -> (recorded time True (newCall cid Ringing time video), [HangUpCall cid])
  CallPeer cid time video -> only client
    { clientCall = case clientCall client of
        Just c | callContact c == cid -> Just c
          { callPeerVideo = video
          , callPhase = if callPhase c == Calling then Talking time else callPhase c
          }
        other -> other
    }
  CallEnded cid time -> only $ case clientCall client of
    Just c | callContact c == cid -> (recorded time (callPhase c == Ringing) c) { clientCall = Nothing }
    _ -> client
  CallGlared cid time -> case clientCall client of
    Just c | callContact c == cid, callPhase c == Calling, callGlare c == Clear ->
      only client { clientCall = Just c { callGlare = Glared time } }
    _ -> ownUpdate viewing (CallEnded cid time) client
  Tick time -> case clientCall client of
    Just c
      | Glared since <- callGlare c
      , let again = Text.toUpper (Text.take 64 (profileToxId (clientProfile client))) > Text.toUpper key
            ContactId key = callContact c
      , diffUTCTime time since >= (if again then glareDelay else glareWait) ->
          if again
            then (client { clientCall = Just c { callGlare = Settling } }, [CallFriend (callContact c) (callCamera c)])
            else only (recorded time False c) { clientCall = Nothing }
    Just c
      | callPhase c `elem` [Calling, Ringing]
      , diffUTCTime time (callStarted c) >= ringTimeout ->
          ((recorded time (callPhase c == Ringing) c) { clientCall = Nothing }, [HangUpCall (callContact c)])
    _ -> only client
  SendPictures cid way -> only (adjusted cid (\c -> c { contactPictures = way }))
  Invited cid gid kind name bytes
    | Map.member cid (clientContacts client)
    , maybe True ((`notElem` [Joining, InGroup]) . groupPhase) (Map.lookup gid (clientGroups client)) ->
        only client { clientInvites = Map.insert (cid, gid) (Invite kind name bytes Nothing) (clientInvites client) }
    | otherwise -> only client
  AnswerInvite cid gid answer -> case Map.lookup (cid, gid) (clientInvites client) of
    Nothing -> only client
    Just invite -> case answer of
      Nothing -> only client { clientInvites = Map.delete (cid, gid) (clientInvites client) }
      Just password ->
        let online = maybe False contactOnline (Map.lookup cid (clientContacts client))
            -- Keep the chat of a group we left before.
            before = Map.lookup gid (clientGroups client)
            joining = (fromMaybe (newGroup (inviteName invite) (inviteKind invite) Joining) before)
              { groupPhase = Joining, groupMembers = Map.empty, groupSaved = Nothing }
        in ( client
               { clientInvites = if online then Map.delete (cid, gid) (clientInvites client)
                                 else Map.insert (cid, gid) invite { inviteAnswered = Just password } (clientInvites client)
               , clientGroups = Map.insert gid joining (clientGroups client) }
           , [joinCommand cid gid invite password | online] )
  GroupConnected gid time -> case Map.lookup gid (clientGroups client) of
    Just g | groupPhase g == Joining ->
      ( withGroup gid g { groupPhase = InGroup, groupSince = Just time }
      , [SetGroupStatus gid away | let away = profileAway (clientProfile client), away /= Tox.Online]
        -- Messages written while we weren't in it.
        ++ [ SendGroupMessage gid i text | GroupEntry _ (Said Nothing _ text Pending) i <- groupEntries g ] )
    _ -> only client
  GroupRefused gid why -> case Map.lookup gid (clientGroups client) of
    Just g | groupPhase g `elem` [Joining, InGroup] ->
      -- Quit, or Tox keeps the group around for nothing.
      (withGroup gid g { groupPhase = Refused why, groupMembers = Map.empty, groupSaved = Nothing }, [QuitGroup gid])
    _ -> only client
  MemberJoined gid time n member -> only $ noting gid $ \g ->
    (g { groupMembers = Map.insert n member (groupMembers g) }, [GroupEntry time (Arrived member) | arrived g time])
  MemberLeft gid time n how -> only $ noting gid $ \g ->
    ( g { groupMembers = Map.delete n (groupMembers g), groupStatuses = Map.delete n (groupStatuses g) }
    , [ GroupEntry time (Departed member)
      | groupPhase g == InGroup, how `notElem` [Tox.ExitSelfDisconnected, Tox.ExitKicked]
      , Just member <- [Map.lookup n (groupMembers g)] ] )
  MembersListed gid time listed -> case Map.lookup gid (clientGroups client) of
    Just g ->
      let keys = map memberKey . Map.elems
          now = Map.fromList listed
          came = [m | m <- Map.elems now, memberKey m `notElem` keys (groupMembers g)]
          went = [m | m <- Map.elems (groupMembers g), memberKey m `notElem` keys now]
      in ( noting gid (const
             ( g { groupMembers = now }
             -- Silent for the first list after getting in (they were
             -- already there).
             , [ GroupEntry time e
               | arrived g time, not (Map.null (groupMembers g)), e <- map Arrived came ++ map Departed went ] ))
         -- Messages that went to nobody.
         , [ SendGroupMessage gid i text
           | groupPhase g == InGroup, not (Map.null now)
           , GroupEntry _ (Said Nothing _ text Pending) i <- groupEntries g ] )
    Nothing -> only client
  MemberNamed gid n name -> only $ adjustedGroup gid $ \g -> g
    { groupMembers = Map.adjust (\m -> m { memberName = name }) n (groupMembers g) }
  GroupReceived gid time n kind text -> only $ noting gid $ \g ->
    ( g { groupUnread = groupUnread g + (if reading (GroupChat gid) then 0 else 1) }
    , [GroupEntry time (Said (Just (memberOf g n)) kind text Delivered)] )
  GroupSent gid i -> only $ adjustedGroup gid $ \g -> g
    { groupEntries = [ case e of
                         GroupEntry t (Said Nothing kind text Pending) j | j == i -> GroupEntry t (Said Nothing kind text Sent) j
                         GroupEntry t (PrivateTo who kind text Pending) j | j == i -> GroupEntry t (PrivateTo who kind text Sent) j
                         _ -> e
                     | e <- groupEntries g ] }
  PrivateReceived gid time n kind text -> only $ noting gid $ \g ->
    ( g { groupUnread = groupUnread g + (if reading (GroupChat gid) then 0 else 1) }
    , [GroupEntry time (PrivateFrom (memberOf g n) kind text)] )
  RolesKnown gid self theirs -> only $ adjustedGroup gid $ \g -> g
    { groupSelfRole = self, groupRoles = Map.fromList theirs }
  MemberStatus gid n status -> only $ adjustedGroup gid $ \g -> g
    { groupStatuses = if status == Tox.Online then Map.delete n (groupStatuses g) else Map.insert n status (groupStatuses g) }
  GroupModerated gid time setter target moderation -> case Map.lookup gid (clientGroups client) of
    Just g ->
      let notice = GroupEntry time (Moderated (memberOf g <$> setter) (memberOf g <$> target) moderation)
          kicked = isNothing target && moderation == Tox.ModKick
      in if kicked
           -- Quit, or Tox keeps the group around for nothing.
           then ( noting gid (const (g { groupPhase = Kicked, groupMembers = Map.empty, groupSaved = Nothing }, [notice]))
                , [QuitGroup gid] )
           else only (noting gid (const (g, [notice])))
    Nothing -> only client
  SettingsKnown gid settings -> only $ adjustedGroup gid (\g -> g { groupSettings = Just settings })
  Moderate gid n permission -> (client, [ModerateMember gid n permission])
  ChangeSettings gid settings -> (client, [SetGroupSettings gid settings])
  ChooseRelayBot gid bot -> only $ adjustedGroup gid (\g -> g { groupRelayBot = bot })
  ToggleMentions gid -> only $ adjustedGroup gid (\g -> g { groupMentions = not (groupMentions g) })
  TopicChanged gid time setter topic -> only $ noting gid $ \g ->
    ( g { groupTopic = topic }
    -- Silent for the topic Tox gets while joining.
    , [ GroupEntry time (TopicSet (Just (maybe (Member "" "") (memberOf g) setter)) topic)
      | arrived g time, topic /= groupTopic g ] )
  TopicSetByUs gid time topic -> only $ noting gid $ \g -> (g { groupTopic = topic }, [GroupEntry time (TopicSet Nothing topic)])
  TopicAllowed gid allowed -> only $ adjustedGroup gid (\g -> g { groupMayTopic = allowed })
  SetTopic gid topic
    | Just g <- Map.lookup gid (clientGroups client), groupPhase g == InGroup, groupMayTopic g ->
        (client, [SetGroupTopic gid (Text.strip topic)])
    | otherwise -> only client
  Found founding name -> (client, [FoundGroup founding (Text.strip name) (groupNick (clientProfile client))])
  GroupCreated gid kind name time ->
    only (withGroup gid (newGroup name kind InGroup) { groupSince = Just time, groupSelfRole = Just Tox.Founder })
  JoinPublic gid password ->
    let joining = maybe True ((`notElem` [Joining, InGroup]) . groupPhase) (Map.lookup gid (clientGroups client))
        fresh = newGroup "" NgcGroup Joining
    in ( client { clientGroups = if joining
                    then Map.insert gid (maybe fresh (\g -> g { groupPhase = Joining, groupSaved = Nothing })
                                           (Map.lookup gid (clientGroups client))) (clientGroups client)
                    else clientGroups client }
       , [JoinPublicGroup gid (groupNick (clientProfile client)) password | joining] )
  InviteFriend gid cid -> (client, [InviteToGroup gid cid])
  GroupSaved gid saved -> only $ adjustedGroup gid $ \g ->
    if groupPhase g `elem` [Joining, InGroup] then g { groupSaved = Just saved } else g
  GroupNamed gid name -> only $ adjustedGroup gid $ \g -> g { groupName = name }
  LeaveGroup gid -> case Map.lookup gid (clientGroups client) of
    Just g | groupPhase g `elem` [Joining, InGroup] ->
      (withGroup gid g { groupPhase = Gone, groupMembers = Map.empty, groupSaved = Nothing }, [QuitGroup gid])
    _ -> only client
  DeleteGroup gid -> case Map.lookup gid (clientGroups client) of
    Just g ->
      ( client { clientGroups = Map.delete gid (clientGroups client)
               , clientGroupDrafts = Map.delete gid (clientGroupDrafts client) }
      , [QuitGroup gid | groupPhase g `elem` [Joining, InGroup]] )
    Nothing -> only client
  MuteGroup gid until' -> only $ adjustedGroup gid (\g -> g { groupMute = Just (maybe MutedAlways MutedUntil until') })
  UnmuteGroup gid -> only $ adjustedGroup gid (\g -> g { groupMute = Nothing })
  where
    only c = (c, [])
    reading chat = viewingShown viewing && viewingChat viewing == Just chat
    newContact = Contact Nothing False [] 0 Nothing "" Tox.Online Nothing Nothing Nothing False
    ours text time file = Message True text time Pending file Nothing 0 Nothing Tox.MessageNormal Nothing
    messageOf cid i = find ((== i) . messageId) . contactMessages =<< Map.lookup cid (clientContacts client)
    known cid = fromMaybe newContact (Map.lookup cid (clientFormer client))
    entry c record = Message (callMine c) "" (callStarted c) Delivered Nothing (Just record) 0 Nothing Tox.MessageNormal Nothing
    begun c = using 1 $ (adjusted (callContact c) $ \contact -> grown (callContact c) contact
        { contactMessages = contactMessages contact ++ [entry c (CallRecord (callVideo c) Nothing False) newId] })
      { clientCall = Just c { callEntry = Just newId } }
    recorded time missed c = using (maybe 1 (const 0) (callEntry c)) $ adjusted (callContact c) $ \contact -> grown (callContact c) contact
      { contactMessages = case callEntry c of
          Just i  -> [ if messageId m == i then entry c record i else m | m <- contactMessages contact ]
          Nothing -> contactMessages contact ++ [entry c record newId]
      , contactUnread = contactUnread contact + (if missed && not (reading (FriendChat (callContact c))) then 1 else 0)
      }
      where
        record = CallRecord (callVideo c) talked missed
        talked = case callPhase c of
          Talking since -> Just (diffUTCTime time since)
          _             -> Nothing
    groupSend gid to time = case (Map.lookup gid (clientGroups client), Text.strip <$> Map.lookup gid (clientGroupDrafts client)) of
      (Just g, Just text)
        | groupPhase g `elem` [Joining, InGroup], not (Text.null text) ->
            let i = newId
                sent written commands =
                  ( using 1 $ (withGroup gid (grownGroup gid g { groupEntries = groupEntries g ++ [GroupEntry time written i] }))
                      { clientGroupDrafts = Map.delete gid (clientGroupDrafts client) }
                  , commands )
            in case to of
                 Nothing -> sent (Said Nothing Tox.MessageNormal text Pending) [SendGroupMessage gid i text | groupPhase g == InGroup]
                 Just key | Just (n, m) <- memberByKey key g -> sent (PrivateTo m Tox.MessageNormal text Pending) [SendPrivate gid i n text]
                 -- Never send a private message to everyone if the member is
                 -- gone.
                 Just _ -> only client
      _ -> only client
    withContact cid contact = client { clientContacts = Map.insert cid contact (clientContacts client) }
    withGroup gid g = client { clientGroups = Map.insert gid g (clientGroups client) }
    adjustedGroup gid f = client { clientGroups = Map.adjust f gid (clientGroups client) }
    -- Changes a group and adds entries to its chat.
    noting gid f = case Map.lookup gid (clientGroups client) of
      Just g -> let (g', added) = f g
                in using (length added) (withGroup gid (grownGroup gid g' { groupEntries = groupEntries g' ++ zipWith ($) added newIds }))
      Nothing -> client
    -- Chats not open keep their newest, and those still in use.
    grown cid c = if viewingChat viewing == Just (FriendChat cid) then c else trimmed cid c
    grownGroup gid g = if viewingChat viewing == Just (GroupChat gid) then g else trimmedGroup g
    trimmed cid c =
      let kept = newest (viewingPage viewing) (held cid) (contactMessages c)
      in c { contactMessages = kept, contactEarlier = contactEarlier c || length kept < length (contactMessages c) }
    trimmedGroup g =
      let kept = newest (viewingPage viewing) entryInUse (groupEntries g)
      in g { groupEntries = kept, groupEarlier = groupEarlier g || length kept < length (groupEntries g) }
    held cid m = inUse m || Just (messageId m) `elem` [callEntry =<< clientCall client, Map.lookup cid (clientReplies client)]
    -- IDs for what the event adds, in order. 'using' counts those taken.
    newIds = unused (clientIds client)
    newId = nextId (clientIds client)
    using :: Int -> Client -> Client
    using n c = c { clientIds = (clientIds client) { idsUsed = idsUsed (clientIds client) + fromIntegral n } }
    joinCommand cid gid invite password = case inviteKind invite of
      NgcGroup -> JoinGroup cid gid (inviteData invite) (groupNick (clientProfile client)) password
      _        -> JoinConference cid gid (inviteData invite)
    -- Joined after Tox met the peers already there.
    arrived g time = groupPhase g == InGroup && maybe False ((>= meeting) . diffUTCTime time) (groupSince g)
    memberOf g n = Map.findWithDefault (Member "" "") n (groupMembers g)
    adjusted cid f = client { clientContacts = Map.adjust f cid (clientContacts client) }
    fileOf cid i = messageFile =<< messageOf cid i
    liveRef cid i = case [ref | ((c, ref), j) <- Map.toList (clientTransfers client), c == cid, j == i] of
      ref : _ -> Just ref
      []      -> Nothing

-- Linked accounts

-- | For linked profiles. What the user does becomes events for the home,
-- and the chats are derived from the account's events. Our own Tox only
-- has the home as a friend.
linkedUpdate :: Linked -> Viewing -> Event -> Client -> (Client, [Command])
linkedUpdate l viewing event client = case event of
  Started _ ->
    ( linkedView l client
    , [maybe (Befriend home) (RequestFriend (linkedHomeAddress l)) (Linked.linkedPairing l)] )
  FriendOnline cid online
    | cid == home && online -> (client, [SendSync home (Linked.homeOnline l)])
    | cid == home -> linked (Linked.homeOffline l) client []
  SyncReceived cid packet
    | cid == home ->
        let (replies, asked, l') = Linked.receivePacket packet l
            (c, commands) = linked l' client replies
        in (c, commands ++ [ServeBlob home blob (Linked.noBlob (Sync.BlobId blob)) | Sync.BlobId blob <- asked])
  BlobMissing blob -> linked (Linked.blobMissing (Sync.BlobId blob) l) client []
  BlobArrived blob path small ->
    let here = Linked.blobHere (Sync.BlobId blob) path l
        avatars = case small of
          Just png | blob `elem` avatarBlobs l -> Map.insert blob png (clientAvatars client)
          _                                    -> clientAvatars client
    in linked here client { clientAvatars = avatars } []
  -- The home accepts theirs, then we fetch it from the home.
  Download cid i | Just (fid, file) <- linkedFileAt l i -> case linkedEnd l fid of
    Just (True, Just blob) ->
      let (l', commands) = fetchFile l blob file in (fst (linked l' client []), commands)
    Nothing | Map.notMember fid (linkedAnswers l) ->
      let (l', packets) = authored (friendGraphOf cid) now (Sync.FileAnswered fid True) l in linked l' client packets
    _ -> only client
  Cancel cid i | Just (fid, _) <- linkedFileAt l i, isNothing (linkedEnd l fid) ->
    let (l', packets) = authored (friendGraphOf cid) now (Sync.FileAnswered fid False) l in linked l' client packets
  -- Staged items first, then the text. Files go once stored as blobs.
  Send (FriendChat cid) time
    | let staged = Map.findWithDefault [] cid (clientStaged client)
          draft = maybe "" Text.strip (Map.lookup cid (clientDrafts client))
    , not (null staged) || not (Text.null draft) ->
        let replied = Map.lookup cid (clientReplies client) >>= \i ->
              find ((== i) . messageId) . contactMessages =<< Map.lookup cid (clientContacts client)
            text = maybe id (\m t -> quoted m <> "\n" <> t) replied draft
            g = friendGraphOf cid
            said = [locationText latitude longitude | StagedPlace latitude longitude <- staged]
              ++ [text | not (Text.null draft)]
            (l', packets) = foldl (\(l0, ps) t ->
                                     let (l1, p) = authored g time (Sync.Send (senderIn g) (Text.encodeUtf8 t) Sync.Normal) l0
                                     in (l1, ps ++ p)) (l, []) said
            (sending, commands) = linked l' client
              { clientDrafts = Map.delete cid (clientDrafts client)
              , clientStaged = Map.delete cid (clientStaged client)
              , clientReplies = if Text.null draft then clientReplies client else Map.delete cid (clientReplies client) }
              packets
        in (sending, [ShareFile cid path name size | StagedFile path name size _ <- staged] ++ commands)
  Send (GroupChat gid) time -> groupSend gid time (\g bytes -> Just (Sync.Send (senderIn g) bytes Sync.Normal))
  SendPrivately gid key time -> groupSend gid time $ \g bytes -> do
    member <- memberByKey key =<< Map.lookup gid (clientGroups client)
    pk <- keyOf (memberKey (snd member))
    pure (Sync.SendPrivately pk (senderIn g) bytes Sync.Normal)
  AddFriend address message | Just pk <- keyOf (Text.take 64 address) ->
    let nospam = nospamOf (Text.take 8 (Text.drop 64 address))
        (l', packets) = authored account now (Sync.WantFriend pk (Just (Text.strip message)) nospam) l
    in linked l' client packets
  AcceptRequest cid -> friendWish cid True
  RejectRequest cid -> friendWish cid False
  DeleteContact cid _ -> friendWish cid False
  MuteChat cid until' -> setting muteKind (friendGraphOf cid) (Just (muteOf until' True))
  UnmuteChat cid -> setting muteKind (friendGraphOf cid) Nothing
  -- Sending pictures as they are is the default, so clear the setting.
  SendPictures cid way -> setting picturesKind (friendGraphOf cid) (Sync.PictureFormat <$> (syncPictures =<< way))
  MuteGroup gid until' | Just c <- chatOf gid ->
    setting muteKind (Sync.chatGraph c) (Just (muteOf until' (maybe True groupMentions (Map.lookup gid (clientGroups client)))))
  UnmuteGroup gid | Just c <- chatOf gid -> setting muteKind (Sync.chatGraph c) Nothing
  ToggleMentions gid
    | Just c <- chatOf gid, Just g <- Map.lookup gid (clientGroups client), Just m <- groupMute g ->
        setting muteKind (Sync.chatGraph c) (Just (muteOf (case m of MutedUntil t -> Just t; MutedAlways -> Nothing)
                                                 (not (groupMentions g))))
  AnswerInvite _ (GroupId invite) answer
    | Just i <- Sync.EventId <$> unhex' invite ->
        let password = mfilter (not . Text.null) answer
            (l', packets) = authored account now (Sync.AnswerInvite i (isJust answer) password) l
        in linked l' client packets
  LeaveGroup gid | Just c <- chatOf gid -> leaving c
  -- We only show chats the account is in, so deleting one leaves it.
  DeleteGroup gid | Just c <- chatOf gid -> leaving c
  ChangeProfile nickname status ->
    let (l1, p1) = authored account now (Sync.Wish Sync.AccountSubject (Sync.Name (Text.strip nickname))) l
        (l2, p2) = authored account now (Sync.Wish Sync.AccountSubject (Sync.StatusMessage (Text.strip status))) l1
    in linked l2 client (p1 ++ p2)
  SetAway away ->
    let (l', packets) = authored account now (Sync.Wish Sync.AccountSubject (Sync.Status (toEnum (fromEnum away)))) l
    in linked l' client packets
  SetTopic gid topic | Just c <- chatOf gid ->
    let (l', packets) = authored account now (Sync.Wish (Sync.ChatSubject c) (Sync.Topic (Text.strip topic))) l
    in linked l' client packets
  ChooseRelayBot gid bot | Just c <- chatOf gid ->
    case (bot, Map.lookup c (Sync.accountRelayBots (Linked.accountState l))) of
      (Just key, _) | Just pk <- keyOf key ->
        let (l', packets) = authored account now (Sync.RelayBot c pk True) l in linked l' client packets
      (Nothing, Just pk) ->
        let (l', packets) = authored account now (Sync.RelayBot c pk False) l in linked l' client packets
      _ -> only client
  -- From our own Tox, whose only friend is the home.
  FriendNamed {} -> only client
  FriendStatusMessage {} -> only client
  FriendStatus {} -> only client
  FriendAvatar {} -> only client
  Received {} -> only client
  Requested {} -> only client
  AddFailed {} -> only client
  Invited {} -> only client
  FileShared cid time name size blob path ->
    let g = friendGraphOf cid
        (l', packets) = authored g time (Sync.File (senderIn g) Nothing name size False (Just (Sync.BlobId blob))) l
    in linked (Linked.blobHere (Sync.BlobId blob) path l') client packets
  SetAvatar png ->
    let hash = SHA256.hash png
        (l', packets) = authored account now (Sync.Wish Sync.AccountSubject (Sync.Avatar (Just (Sync.BlobId hash)))) l
        (c, commands) = linked l' client { clientAvatars = Map.insert hash png (clientAvatars client) } packets
    in (c, StoreBlob png : commands)
  RemoveAvatar ->
    let (l', packets) = authored account now (Sync.Wish Sync.AccountSubject (Sync.Avatar Nothing)) l
    in linked l' client packets
  Found founding name ->
    let (kind, privacy) = case founding of
          PrivateGroup  -> (Sync.GroupKind, Sync.Private)
          PublicGroup   -> (Sync.GroupKind, Sync.Public)
          NewConference -> (Sync.ConferenceKind, Sync.Public)
        (l', packets) = authored account now (Sync.NewChat kind (Text.strip name) privacy) l
    in linked l' client packets
  JoinPublic (GroupId chat) password | Just chatId <- unhex' chat ->
    let (l', packets) = authored account now (Sync.JoinChat chatId (mfilter (not . Text.null) (Just password))) l
    in linked l' client packets
  InviteFriend gid (ContactId key) | Just c <- chatOf gid, Just friend <- keyOf key ->
    let (l', packets) = authored account now (Sync.InviteToChat c friend) l
    in linked l' client packets
  -- The home answers or places the friend's call, then calls us and relays
  -- it.
  AnswerCall time camera -> case clientCall client of
    Just c | callPhase c == Ringing ->
      viaHome (client { clientCall = Just c { callPhase = Calling, callStarted = time, callCamera = camera } }, [CallFriend (callContact c) camera])
    _ -> only client
  StartCall {} -> viaHome (ownUpdate viewing event client)
  HangUp {} -> viaHome (ownUpdate viewing event client)
  ToggleCamera -> viaHome (ownUpdate viewing event client)
  CallElsewhere {} -> viaHome (ownUpdate viewing event client)
  -- The home gives up on a ringing call, not us.
  Tick {}
    | (callPhase <$> clientCall client) == Just Ringing -> only client
    | otherwise -> viaHome (ownUpdate viewing event client)
  CallInvited cid time video | cid == home, Just friend <- relaying -> viaHome (ownUpdate viewing (CallInvited friend time video) client)
  CallPeer cid time video | cid == home, Just friend <- relaying -> viaHome (ownUpdate viewing (CallPeer friend time video) client)
  CallEnded cid time | cid == home, Just friend <- relaying -> viaHome (ownUpdate viewing (CallEnded friend time) client)
  CallGlared cid time | cid == home, Just friend <- relaying -> viaHome (ownUpdate viewing (CallEnded friend time) client)
  Moderate {} -> wishing
  ChangeSettings {} -> wishing
  _ -> readToEnd viewing l (fst (ownUpdate viewing event client)) []
  where
    only c = (c, [])
    linked = withLinked viewing
    leaving c =
      let (l', packets) = authored account now (Sync.Wish (Sync.ChatSubject c) (Sync.Joined False)) l
      in linked l' client packets
    home = ContactId (Text.toUpper (Text.take 64 (linkedHomeAddress l)))
    account = Sync.accountGraph (Linked.accountKey l)
    now = linkedClock l
    friendGraphOf (ContactId key) = Sync.friendGraph (Linked.accountKey l) (fromMaybe (Sync.PublicKey BS.empty) (keyOf key))
    chatOf gid = listToMaybe [c | c <- Sync.joinedChats (Linked.accountState l), chatGroupId c == gid]
    senderIn = linkedSender l
    groupSend gid time content
      | Just c <- chatOf gid
      , Just draft <- Text.strip <$> Map.lookup gid (clientGroupDrafts client), not (Text.null draft)
      , Just made <- content (Sync.chatGraph c) (Text.encodeUtf8 draft) =
          let (l', packets) = authored (Sync.chatGraph c) time made l
          in linked l' client { clientGroupDrafts = Map.delete gid (clientGroupDrafts client) } packets
      | otherwise = only client
    relaying = case clientCall client of
      Just c | callVia c == Just home -> Just (callContact c)
      _                               -> Nothing
    -- Call commands go to the home, which relays the call. We ask it to
    -- place new ones.
    viaHome (c, commands) =
      let c' = c { clientCall = (\call -> call { callVia = Just home }) <$> clientCall c }
          toHome = \case
            CallFriend (ContactId key) camera | Just pk <- keyOf key -> [SendSync home (Linked.callAction pk (Protocol.Connect camera))]
            AnswerFriend _ camera -> [AnswerFriend home camera]
            HangUpCall (ContactId key) ->
              HangUpCall home : [SendSync home (Linked.callAction pk Protocol.HangUp) | Just pk <- [keyOf key]]
            CameraCall _ camera -> [CameraCall home camera]
            other -> [other]
      in (c', concatMap toHome commands)
    -- Turn Tox commands into wishes for the home.
    wishing =
      let (c, commands) = ownUpdate viewing event client
          (l', packets) = foldl (\(l0, ps) (subject', value') ->
                                   let (l1, p) = authored account now (Sync.Wish subject' value') l0 in (l1, ps ++ p))
                                (l, []) (concatMap wishesOf commands)
      in linked l' c packets
    wishesOf = \case
      ModerateMember gid n action
        | Just c <- chatOf gid
        , Just pk <- keyOf . memberKey =<< Map.lookup n . groupMembers =<< Map.lookup gid (clientGroups client) ->
            [(Sync.PeerSubject c pk, case action of
                MakeModerator -> Sync.RoleValue Sync.Moderator
                MakeUser      -> Sync.RoleValue Sync.User
                MakeObserver  -> Sync.RoleValue Sync.Observer
                Kick          -> Sync.Present False)]
      SetGroupSettings gid new
        | Just c <- chatOf gid, Just old <- groupSettings =<< Map.lookup gid (clientGroups client) ->
            let differs f = f old /= f new
            in [ (Sync.ChatSubject c, v)
               | (True, v) <-
                   [ (differs Tox.groupPrivate, Sync.PrivacyValue (if Tox.groupPrivate new then Sync.Private else Sync.Public))
                   , (differs Tox.groupPeerLimit, Sync.PeerLimit (fromIntegral (Tox.groupPeerLimit new)))
                   , (differs Tox.groupPassword, Sync.Password (Text.decodeUtf8Lenient (Tox.groupPassword new)))
                   , (differs Tox.groupTopicLock, Sync.TopicLock (Tox.groupTopicLock new))
                   , (differs Tox.groupVoice, Sync.VoiceStateValue (syncVoice (Tox.groupVoice new))) ] ]
      _ -> []
    syncVoice = \case
      Tox.VoiceAll        -> Sync.VoiceEveryone
      Tox.VoiceModerators -> Sync.VoiceModerators
      Tox.VoiceFounder    -> Sync.VoiceFounder
    friendWish (ContactId key) wanted = case keyOf key of
      Just pk ->
        let content = if wanted then Sync.WantFriend pk Nothing Nothing else Sync.UnwantFriend pk
            (l', packets) = authored account now content l
        in linked l' client packets
      Nothing -> only client
    -- Mute or picture format of a conversation. Keep any fields that newer
    -- clients added.
    setting kind g s' =
      let scope = Sync.ConversationScope g
          old = Map.lookup kind =<< Map.lookup scope (Sync.accountSettings (Linked.accountState l))
          content = maybe (Sync.ClearSetting scope (fromIntegral kind))
            (\new -> Sync.SettingContent scope (maybe (Sync.kept new) (Sync.withSetting new) old)) s'
          (l', packets) = authored account now content l
      in linked l' client packets
    muteOf until' = Sync.Mute (fromIntegral . Linked.toMillis <$> until')
    nospamOf t = case unhex' t of
      Just bytes | BS.length bytes == 4 -> Just (BS.foldl' (\n b -> n * 256 + fromIntegral b) 0 bytes)
      _ -> Nothing

muteKind, picturesKind :: Word64
muteKind = Sync.settingKind (Sync.Mute Nothing True)
picturesKind = Sync.settingKind (Sync.PictureFormat Sync.Original)

-- | Our picture format key as a sync encoding ('picturesOf' turns it back).
syncPictures :: Text -> Maybe Sync.PictureEncoding
syncPictures key
  | key == Pictures.formatKey Pictures.qoi = Just Sync.Qoi
  | otherwise = Nothing

picturesOf :: Sync.PictureEncoding -> Maybe Text
picturesOf = \case
  Sync.Qoi      -> Just (Pictures.formatKey Pictures.qoi)
  Sync.Original -> Nothing

withLinked :: Viewing -> Linked -> Client -> [BS.ByteString] -> (Client, [Command])
withLinked viewing l client = readToEnd viewing l (linkedView l client { clientLinked = Just l })

-- | Mark the open chat read to its end, as an event for the other devices.
readToEnd :: Viewing -> Linked -> Client -> [BS.ByteString] -> (Client, [Command])
readToEnd viewing l0 client0 packets = case unreadEnd of
  Just (g, i, rank) ->
    let (l', more) = authored (Sync.accountGraph (Linked.accountKey l)) (linkedClock l) (Sync.ReadUpTo g i rank) l
    in (linkedView l' client { clientLinked = Just l' }, send (packets ++ more) ++ fetches)
  Nothing -> (client, send packets ++ fetches)
  where
    (l, fetches) = fetching l0 client0
    client = client0 { clientLinked = Just l }
    home = ContactId (Text.toUpper (Text.take 64 (linkedHomeAddress l)))
    send ps = [SendSync home ps | not (null ps)]
    store = Replica.replicaStore (Linked.linkedReplica l)
    shown g = viewingShown viewing && viewingEnd viewing && case viewingChat viewing of
      Just (FriendChat (ContactId key)) -> Just g == (Sync.friendGraph (Linked.accountKey l) <$> keyOf key)
      Just (GroupChat gid) -> g `elem` [Sync.chatGraph c | c <- Sync.joinedChats (Linked.accountState l), chatGroupId c == gid]
      Nothing -> False
    unreadEnd = listToMaybe
      [ (g, i, Sync.eventRank e)
      | g <- Sync.storeGraphs store, shown g
      , (i, e) <- take 1 (reverse (Sync.byRank g store))
      , unreadIn (Linked.accountState l) g e ]

-- | Whether no device has read the event yet. Read up to a rank means
-- everything of that rank or lower.
unreadIn :: Sync.AccountState -> Sync.GraphId -> Sync.Event -> Bool
unreadIn state g e = maybe True ((< Sync.eventRank e) . fst) (Map.lookup g (Sync.accountRead state))

-- | Ask the home for avatar blobs we don't have (once per session), and for
-- accepted files.
fetching :: Linked -> Client -> (Linked, [Command])
fetching l client = foldl (\(l', commands) (blob, file) -> (<>) commands <$> fetchFile l' blob file)
  (foldl ask (l, []) missing) files
  where
    missing = [ blob | Linked.inSession l, blob <- avatarBlobs l, Map.notMember blob (clientAvatars client)
                     , Set.notMember (Sync.BlobId blob) (Linked.linkedWanted l) ]
    files = [ (blob, file) | (fid, file@(_, size)) <- linkedFiles l
                           , size <= settingAutoAccept (clientAccount client) || Map.lookup fid (linkedAnswers l) == Just True
                           , Just (True, Just blob) <- [linkedEnd l fid] ]
      -- Ours, stored here as blobs (to find them again after a restart).
      ++ linkedOurs l
    home = ContactId (Text.toUpper (Text.take 64 (linkedHomeAddress l)))
    ask (l', commands) blob =
      let (packets, l'') = Linked.wantBlob (Sync.BlobId blob) l' in (l'', commands ++ [FetchBlob home blob packets Nothing])

-- | Use the blob if we have it, else ask the home in a session.
fetchFile :: Linked -> Sync.BlobId -> (Text, Word64) -> (Linked, [Command])
fetchFile l blob@(Sync.BlobId bytes) (name, _)
  | Map.member blob (Linked.linkedHave l) = (l, [])
  | Linked.inSession l, Set.notMember blob (Linked.linkedWanted l) =
      let (packets, l') = Linked.wantBlob blob l in (l', [FetchBlob home bytes packets (Just name)])
  | Set.notMember blob (Linked.linkedLooked l) = (Linked.lookForBlob blob l, [FetchBlob home bytes [] (Just name)])
  | otherwise = (l, [])
  where home = ContactId (Text.toUpper (Text.take 64 (linkedHomeAddress l)))

-- | Files friends sent the account.
linkedFiles :: Linked -> [(Sync.EventId, (Text, Word64))]
linkedFiles l =
  [ (i, (name, size)) | g <- Sync.storeGraphs store, (i, e) <- Sync.byRank g store
                      , Sync.File _ _ name size True _ <- [Sync.eventContent e] ]
  where store = Replica.replicaStore (Linked.linkedReplica l)

-- | Our files sent by the account's devices.
linkedOurs :: Linked -> [(Sync.BlobId, (Text, Word64))]
linkedOurs l =
  [ (blob, (name, size)) | g <- Sync.storeGraphs store, (_, e) <- Sync.byRank g store
                         , Sync.File _ _ name size False (Just blob) <- [Sync.eventContent e] ]
  where store = Replica.replicaStore (Linked.linkedReplica l)

-- | How a file ended, if it did: OK or not, and the home's blob.
linkedEnd :: Linked -> Sync.EventId -> Maybe (Bool, Maybe Sync.BlobId)
linkedEnd l fid = listToMaybe
  [ (ok, blob) | g <- Sync.storeGraphs store, (_, e) <- reverse (Sync.byRank g store)
               , Sync.FileEnded i ok blob <- [Sync.eventContent e], i == fid ]
  where store = Replica.replicaStore (Linked.linkedReplica l)

-- | Latest accept or decline of each file.
linkedAnswers :: Linked -> Map Sync.EventId Bool
linkedAnswers l = Map.fromList
  [ (i, yes) | g <- Sync.storeGraphs store, (_, e) <- Sync.byRank g store, Sync.FileAnswered i yes <- [Sync.eventContent e] ]
  where store = Replica.replicaStore (Linked.linkedReplica l)

-- | A friend's file, by its message.
linkedFileAt :: Linked -> MessageId -> Maybe (Sync.EventId, (Text, Word64))
linkedFileAt l (MessageId i) = case Sync.eventContent <$> Sync.lookupEvent fid (Replica.replicaStore (Linked.linkedReplica l)) of
  Just (Sync.File _ _ name size True _) -> Just (fid, (name, size))
  _                                     -> Nothing
  where fid = Sync.EventId i

avatarBlobs :: Linked -> [BS.ByteString]
avatarBlobs l = nub [blob | (_, Sync.Avatar (Just (Sync.BlobId blob))) <- Sync.allFacts (Sync.accountFacts (Linked.accountState l))]

authored :: Sync.GraphId -> UTCTime -> Sync.Content -> Linked -> (Linked, [BS.ByteString])
authored g time content l = (l', packets)
  where (_, packets, l') = Linked.author g time content l

linkedHomeAddress :: Linked -> Text
linkedHomeAddress = Linked.linkedHome

-- | Latest event time, for our new events (there's no clock here).
linkedClock :: Linked -> UTCTime
linkedClock l = maximum (Linked.linkedSince l : [Linked.eventTime e | g <- Sync.storeGraphs store, (_, e) <- take 1 (reverse (Sync.byRank g store))])
  where store = Replica.replicaStore (Linked.linkedReplica l)

linkedSender :: Linked -> Sync.GraphId -> Text
linkedSender l g = fromMaybe ourName (listToMaybe
  [n | c <- Sync.joinedChats state, Sync.chatGraph c == g, Sync.SelfName n <- Sync.factsOf (Sync.ChatSubject c) facts])
  where
    state = Linked.accountState l
    facts = Sync.accountFacts state
    ourName = fromMaybe "" (listToMaybe [n | Sync.Name n <- Sync.factsOf Sync.AccountSubject facts])

keyOf :: Text -> Maybe Sync.PublicKey
keyOf key = case unhex' key of
  Just bytes | BS.length bytes == 32 -> Just (Sync.PublicKey bytes)
  _                                  -> Nothing

unhex' :: Text -> Maybe BS.ByteString
unhex' = either (const Nothing) Just . Base16.decode . Text.encodeUtf8

hexOf :: BS.ByteString -> Text
hexOf = Text.toUpper . Text.decodeUtf8 . Base16.encode

chatGroupId :: Sync.ChatId -> GroupId
chatGroupId (Sync.GroupChat i)      = GroupId (hexOf i)
chatGroupId (Sync.ConferenceChat i) = GroupId (hexOf i)

-- | Chats, friends, invites and profile of a linked account, from its
-- events and the home's live facts.
linkedView :: Linked -> Client -> Client
linkedView l client = client
  { clientProfile = (clientProfile client)
      { profileNickname = fromMaybe "" (listToMaybe [n | Sync.Name n <- kept Sync.AccountSubject])
      , profileStatus = fromMaybe "" (listToMaybe [t | Sync.StatusMessage t <- kept Sync.AccountSubject])
      , profileAway = maybe Tox.Online (toEnum . fromEnum)
          (listToMaybe [st | Sync.Status st <- kept Sync.AccountSubject])
      , profileToxId = fromMaybe (Text.toUpper (Linked.linkedHome l))
          (listToMaybe [hexOf a | Sync.Address a <- kept Sync.AccountSubject]) }
  , clientAvatar = avatarOf Sync.AccountSubject
  -- Ring while it rings at the home, until a device answers.
  , clientCall = case clientCall client of
      Nothing -> listToMaybe
        [ (newCall (contactIdOf pk) Ringing (linkedClock l) video) { callPeerVideo = video }
        | (Sync.FriendSubject pk, Sync.CallValue (Sync.Ringing video)) <- Sync.allFacts live ]
      Just c | callPhase c == Ringing, not (ringing (callContact c)) -> Nothing
      other -> other
  , clientContacts = Map.fromList
      -- Leave out the account's devices (they're the home's friends too). Leave
      -- us out even before the home lists us.
      [ (contactIdOf pk, contact pk) | (pk, f) <- Map.toList (Sync.accountFriends state), Sync.friendWanted f
      , pk /= Replica.replicaDevice (Linked.linkedReplica l), Map.notMember pk (Sync.accountDevices state) ]
  , clientRequests = Map.fromList
      [ (contactIdOf pk, asked) | (pk, Sync.Friend False (Just asked)) <- Map.toList (Sync.accountFriends state) ]
  , clientGroups = Map.fromList [(chatGroupId c, group c) | c <- Sync.joinedChats state]
  , clientInvites = Map.fromList
      [ ((contactIdOf (Sync.inviteFrom v), GroupId (hexOf i)), invite v)
      | (Sync.EventId i, v) <- Map.toList (Sync.accountInvites state)
      , Sync.inviteStatus v `elem` [Sync.Pending, Sync.Joining] ]
  }
  where
    state = Linked.accountState l
    facts = Sync.accountFacts state
    live = Linked.liveFacts l
    store = Replica.replicaStore (Linked.linkedReplica l)
    account = Linked.accountKey l
    kept s = Sync.factsOf s facts
    avatarOf s = listToMaybe [blob | Sync.Avatar (Just (Sync.BlobId blob)) <- kept s]
    ringing (ContactId key) = or
      [ True | Just pk <- [keyOf key], Sync.CallValue (Sync.Ringing _) <- Sync.factsOf (Sync.FriendSubject pk) live ]
    contactIdOf (Sync.PublicKey pk) = ContactId (hexOf pk)
    idOf (Sync.EventId i) = MessageId i
    memberKey' (Sync.PublicKey pk) = hexOf pk
    toxKind Sync.Action = Tox.MessageAction
    toxKind Sync.Normal = Tox.MessageNormal
    readable' = readable (clientAccount client)
    timeline g = Sync.displayOrder g store
    went g = Set.fromList [i | (_, e) <- timeline g, Sync.Sent i <- [Sync.eventContent e]]
    arrived g = Map.fromList [(i, Linked.eventTime e) | (_, e) <- timeline g, Sync.Delivered i <- [Sync.eventContent e]]
    ended g = Map.fromList [(i, (d, declined)) | (_, e) <- timeline g, Sync.CallEnded i d declined <- [Sync.eventContent e]]
    delivery g i
      | Map.member i (arrived g) = Delivered
      | Set.member i (went g) = Sent
      | otherwise = Pending
    -- Theirs after the last read and since the link (earlier ones were for
    -- the other devices to notify). Only missed calls count.
    unread g theirs = length
      [ () | (i, e) <- timeline g
      , unreadIn state g e
      , Linked.eventTime e >= Linked.linkedSince l
      , theirs (i, e) ]
    theirMessage g = maybe False (\m -> not (messageMine m) && maybe True recordMissed (messageCall m)) . message g
    theirEntry (_, e) = case Sync.eventContent e of
      Sync.Received {}          -> True
      Sync.ReceivedPrivately {} -> True
      _                         -> False
    muteIn g = case Map.lookup muteKind =<< Map.lookup (Sync.ConversationScope g) (Sync.accountSettings state) of
      Just (Sync.Kept (Sync.Mute until' mentioned) _) ->
        Just (maybe MutedAlways (MutedUntil . Linked.fromMillis . fromIntegral) until', mentioned)
      _ -> Nothing
    contact pk = Contact
      { contactName = listToMaybe [n | Sync.Name n <- kept subject]
      , contactOnline = or [c /= Sync.NoConnection | Sync.ConnectionValue c <- Sync.factsOf subject live]
      , contactMessages = mapMaybe (message g) (timeline g)
      , contactUnread = unread g (theirMessage g)
      , contactRequest = Nothing
      , contactStatusMessage = fromMaybe "" (listToMaybe [t | Sync.StatusMessage t <- kept subject])
      , contactStatus = fromMaybe Tox.Online (listToMaybe [toEnum (fromEnum st) | Sync.Status st <- Sync.factsOf subject live])
      , contactMute = fst <$> muteIn g
      , contactAvatar = avatarOf subject
      , contactPictures = case Sync.keptValue <$> (Map.lookup picturesKind =<< Map.lookup (Sync.ConversationScope g) (Sync.accountSettings state)) of
          Just (Sync.PictureFormat way) -> picturesOf way
          _                             -> Nothing
      , contactEarlier = False
      }
      where
        subject = Sync.FriendSubject pk
        g = Sync.friendGraph account pk
    message g (i, e) = case Sync.eventContent e of
      Sync.Received _ _ bytes kind ->
        Just (Message False (readText (readWith readable' bytes)) t Delivered Nothing Nothing 0 Nothing (toxKind kind) (Just bytes) (idOf i))
      Sync.Send _ bytes kind ->
        Just (Message True (Text.decodeUtf8Lenient bytes) t (delivery g i) Nothing Nothing 0 (Map.lookup i (arrived g)) (toxKind kind) Nothing (idOf i))
      Sync.CallStarted _ incoming video ->
        let (talked, declined) = case Map.lookup i (ended g) of
              Just (d, no) -> (fromIntegral <$> d, no)
              Nothing      -> (Nothing, False)
            missed = incoming && Map.member i (ended g) && isNothing talked && not declined
        in Just (Message (not incoming) "" t Delivered Nothing
                   (Just (CallRecord video ((/ 1000) <$> talked) missed)) 0 Nothing Tox.MessageNormal Nothing (idOf i))
      Sync.File _ _ name size incoming blob ->
        let end = linkedEnd l i
            held = case end of
              Just (_, b) -> b <|> blob
              Nothing     -> blob
            here = (`Map.lookup` Linked.linkedHave l) =<< held
            path = fromMaybe "" here
            progress = case end of
              Just (False, _) -> Cancelled
              Just (True, _) | isJust here -> Complete
              Just (True, Just b) | Set.member b (Linked.linkedMissing l) -> Cancelled
                                  | Set.member b (Linked.linkedWanted l) -> Moving
              Just (True, _) -> if incoming then Waiting else Complete
              Nothing -> case Map.lookup i (linkedAnswers l) of
                Just True  -> Moving
                Just False -> Cancelled
                Nothing    -> if incoming then Waiting else Moving
            done = if progress == Complete then size else 0
        in Just (Message (not incoming) name t (if isJust end then Delivered else Pending)
                   (Just (Attachment size path done progress Nothing)) Nothing 0 Nothing Tox.MessageNormal Nothing (idOf i))
      _ -> Nothing
      where t = Linked.eventTime e
    group c = (newGroup name (if isGroup then NgcGroup else TextConference) InGroup)
      { groupEntries = mapMaybe (entry g peerName) (timeline g)
      , groupUnread = unread g theirEntry
      , groupMembers = Map.fromList [(n, Member (memberKey' pk) (peerName pk)) | (n, pk) <- peers]
      , groupMute = fst <$> muteIn g
      , groupMentions = maybe True snd (muteIn g)
      , groupTopic = fromMaybe "" (listToMaybe [t | Sync.Topic t <- kept subject])
      , groupMayTopic = not locked || selfRole `elem` [Just Tox.Founder, Just Tox.Moderator]
      , groupRoles = Map.fromList [(n, role) | (n, pk) <- peers, Sync.RoleValue r <- peerFacts pk, let role = toxRole r]
      , groupStatuses = Map.fromList
          [(n, status) | (n, pk) <- peers, Sync.Status st <- peerFacts pk, let status = toEnum (fromEnum st), status /= Tox.Online]
      , groupSelfRole = selfRole
      , groupSettings = Tox.GroupSettings
          <$> listToMaybe [p == Sync.Private | Sync.PrivacyValue p <- kept subject]
          <*> Just (Text.encodeUtf8 (fromMaybe "" (listToMaybe [p | Sync.Password p <- kept subject])))
          <*> listToMaybe [fromIntegral n | Sync.PeerLimit n <- kept subject]
          <*> Just locked
          <*> listToMaybe [toxVoice v | Sync.VoiceStateValue v <- kept subject]
      , groupRelayBot = memberKey' <$> Map.lookup c (Sync.accountRelayBots state)
      }
      where
        g = Sync.chatGraph c
        subject = Sync.ChatSubject c
        isGroup = case c of
          Sync.GroupChat _ -> True
          _                -> False
        name = fromMaybe "" (listToMaybe [n | Sync.Name n <- kept subject])
        locked = or [b | Sync.TopicLock b <- kept subject]
        selfRole = listToMaybe [toxRole r | Sync.RoleValue r <- kept subject]
        peerFacts pk = Sync.factsOf (Sync.PeerSubject c pk) live
        -- A member who left has no facts, so use the name of their last
        -- message.
        peerName pk = fromMaybe "" (listToMaybe [n | Sync.Name n <- peerFacts pk] <|> Map.lookup pk wroteAs)
        wroteAs = Map.fromList $ flip mapMaybe (timeline g) $ \(_, e) -> case Sync.eventContent e of
          Sync.Received (Just pk) said _ _   -> Just (pk, said)
          Sync.ReceivedPrivately pk said _ _ -> Just (pk, said)
          _                                  -> Nothing
        -- Our own numbers (Tox's are on the home).
        peers = zip [0 ..] [pk | (Sync.PeerSubject c' pk, Sync.Present True) <- Sync.allFacts live, c' == c]
    entry g nameOf (i, e) = (\what -> GroupEntry (Linked.eventTime e) what (idOf i)) <$> case Sync.eventContent e of
      Sync.Received from sender bytes kind ->
        Just (Said (Just (Member (maybe "" memberKey' from) sender)) (toxKind kind) (readText (readWith readable' bytes)) Delivered)
      Sync.Send _ bytes kind -> Just (Said Nothing (toxKind kind) (Text.decodeUtf8Lenient bytes) (delivery g i))
      Sync.ReceivedPrivately from sender bytes kind ->
        Just (PrivateFrom (Member (memberKey' from) sender) (toxKind kind) (readText (readWith readable' bytes)))
      Sync.SendPrivately to _ bytes kind ->
        Just (PrivateTo (Member (memberKey' to) (nameOf to)) (toxKind kind) (Text.decodeUtf8Lenient bytes) (delivery g i))
      Sync.Notice t -> Just (Noted t)
      _ -> Nothing
    toxRole = \case
      Sync.Founder   -> Tox.Founder
      Sync.Moderator -> Tox.Moderator
      Sync.User      -> Tox.User
      Sync.Observer  -> Tox.Observer
    toxVoice = \case
      Sync.VoiceEveryone   -> Tox.VoiceAll
      Sync.VoiceModerators -> Tox.VoiceModerators
      Sync.VoiceFounder    -> Tox.VoiceFounder
    invite v = Invite
      { inviteKind = if Sync.inviteKind v == Sync.GroupKind then NgcGroup else TextConference
      , inviteName = Sync.inviteName v
      , inviteData = Sync.inviteData v
      , inviteAnswered = if Sync.inviteStatus v == Sync.Joining then Just (fromMaybe "" (Sync.invitePassword v)) else Nothing
      }

-- | A page from the store in front, without any we have already.
inFront :: Ord k => (a -> k) -> [a] -> [a] -> [a]
inFront key page have = filter ((`Set.notMember` Set.fromList (map key have)) . key) page ++ have

-- | The newest, from the oldest still in use if that's earlier.
newest :: Int -> (a -> Bool) -> [a] -> [a]
newest n used xs = drop (minimum (length xs - n : [i | (i, x) <- zip [0 ..] xs, used x])) xs

-- | Nothing if they've left.
memberByKey :: Text -> Group -> Maybe (Tox.PeerId, Member)
memberByKey key g = find ((== key) . memberKey . snd) (Map.toList (groupMembers g))

adjustMessage :: ContactId -> MessageId -> (Message -> Message) -> Client -> Client
adjustMessage cid i f client = client { clientContacts = Map.adjust contact cid (clientContacts client) }
  where
    contact c = c { contactMessages = [ if messageId m == i then f m else m | m <- contactMessages c ] }

adjustFile :: ContactId -> MessageId -> (Attachment -> Attachment) -> Client -> Client
adjustFile cid i f = adjustMessage cid i (\m -> m { messageFile = f <$> messageFile m })

-- | Transfer that Tox ended early. Both ours and theirs wait to be offered
-- again and resume, but only if they have a file ID.
interrupted :: Message -> Message
interrupted m = m { messageFile = cut <$> messageFile m }
  where
    cut a
      | attachmentState a `notElem` [Waiting, Moving] = a
      | messageMine m && (attachmentState a == Waiting || resumable) = a { attachmentState = Waiting }
      | resumable && attachmentState a == Moving = a { attachmentState = Interrupted }
      | resumable = a
      | otherwise = a { attachmentState = Cancelled }
      where resumable = isJust (attachmentId a)

pruned :: Client -> Client
pruned client = client
  { clientAvatars = Map.restrictKeys (clientAvatars client) $ Set.fromList $
      maybe id (:) (clientAvatar client)
        [ h | c <- Map.elems (clientContacts client) ++ Map.elems (clientFormer client), Just h <- [contactAvatar c] ] }

