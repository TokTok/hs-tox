-- | Our account as a .tox file for other clients: keys, profile and
-- contacts, offline friends' names too.
module Tox.Client.Export
  ( exportSave
  ) where

import qualified Crypto.Saltine.Class    as Sodium
import qualified Data.Binary             as Binary
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Base16
import qualified Data.ByteString.Lazy    as LBS
import           Data.Either             (fromRight)
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Word               (byteSwap32)
import qualified Tox.Application.Save    as Save
import           Tox.Client
import           Tox.Client.Types
import qualified Tox.Crypto.Core.KeyPair as KeyPair
import qualified Tox.Session.Friend      as Messenger

-- | Nothing if our key doesn't parse.
exportSave :: Identity -> Client -> Maybe BS.ByteString
exportSave (Identity secret nospam) client = do
  sk <- Sodium.decode secret
  pure . LBS.toStrict . Binary.encode . Save.toSaveData $ Save.Profile
    { Save.profileKeys = KeyPair.fromSecretKey sk
      -- Saved the other way round from what the API gives.
    , Save.profileNospam = byteSwap32 nospam
    , Save.profileName = utf8 (profileNickname (clientProfile client))
    , Save.profileStatusMessage = utf8 (profileStatus (clientProfile client))
    , Save.profileStatus = profileAway (clientProfile client)
    , Save.profileFriends =
        [ Save.SavedFriend
            { Save.sfPublicKey = pk
            , Save.sfRequest = request <$> contactRequest c
            , Save.sfName = maybe BS.empty utf8 (contactName c)
            , Save.sfStatusMessage = utf8 (contactStatusMessage c)
            , Save.sfStatus = Messenger.Online
            , Save.sfLastSeen = 0
            }
        | (ContactId key, c) <- Map.toList (clientContacts client)
        , Right bytes <- [Base16.decode (utf8 key)]
        , Just pk <- [Sodium.decode bytes] ]
    , Save.profileDhtNodes = []
    , Save.profileTcpRelays = []
    , Save.profilePathNodes = []
    , Save.profileConferences = []
    , Save.profileGroups = []
    }
  where
    utf8 = Text.encodeUtf8
    -- Our pending request: nospam from the Tox ID, and the message.
    request (address, message) =
      (fromRight BS.empty (Base16.decode (utf8 (Text.take 8 (Text.drop 64 address)))), utf8 message)
