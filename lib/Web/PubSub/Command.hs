{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.Command
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.Command where

--------------------------------------------------------------------------------
import Data.List.NonEmpty
import Data.Time (UTCTime)

--------------------------------------------------------------------------------
import Web.PubSub.Subscription
import Web.PubSub.Type

--------------------------------------------------------------------------------
data Request
data Verify

--------------------------------------------------------------------------------
data Command a where
    Subscribing   :: Command Request
    Unsubscribing :: Command Request
    VerifySub     :: Sub -> String -> Command Verify

--------------------------------------------------------------------------------
data Error a where
    SubError :: NonEmpty SubError -> Error Request

--------------------------------------------------------------------------------
data Slot
    = forall e.
      Slot
      { slotCallback :: !Callback
      , slotTopic    :: !Topic
      , slotEvent    :: !(Event e)
      , slotDate     :: !UTCTime
      }

--------------------------------------------------------------------------------
data Event a where
    SubEvent        :: Sub -> Event Request
    SubVerified     :: Event Verify
    SubVerifyFailed :: Event Verify
