{-# LANGUAGE OverloadedStrings #-}
module Tox.Client.ChangesSpec (spec) where

import qualified Data.Map.Strict     as Map
import           Test.Hspec
import qualified Tox.Application.Tox as Tox

import           Tox.Client
import           Tox.Client.Changes
import           Tox.Client.Sample
import           Tox.Client.Types
import           Tox.Client.Update

spec :: Spec
spec = do
  it "lists a new message, then that message by its ID as it changes" $ do
    let sent = applying [SetDraft (FriendChat alice) "hi", Send (FriendChat alice) (at 3 9 0)] sample
        i = messageAt alice 3 sent
        delivered = applying [Delivering alice i Sent] sent
    [m | MessageChanged _ m <- changes sample sent] `shouldBe` maybe [] (drop 3 . contactMessages) (Map.lookup alice (clientContacts sent))
    [(cid, messageId m) | MessageChanged cid m <- changes sent delivered] `shouldBe` [(alice, i)]
    changes delivered delivered `shouldBe` []

  it "lists nothing for a chat cut to its newest, or what comes back from the store" $ do
    let small = unseen { viewingPage = 1 }
        trimmed = fst (update small (CloseChat (FriendChat alice)) sample)
        received = fst (update small (Received alice (at 3 9 0) Tox.MessageNormal "more") sample)
    changes sample trimmed `shouldBe` []
    -- Also when the chat was cut down as it came.
    [messageText m | MessageChanged _ m <- changes sample received] `shouldBe` ["more"]
    changes trimmed (applying [EarlierMessages alice (take 2 (maybe [] contactMessages (Map.lookup alice (clientContacts sample))))] trimmed)
      `shouldBe` []
