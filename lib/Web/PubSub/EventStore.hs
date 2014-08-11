{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.EventStore
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.EventStore
    ( EventStore(..)
    , Iterator
    , unverifiedSub
    , verifiedSub
    ) where

--------------------------------------------------------------------------------
import Control.Monad.Trans
import Pipes
import Pipes.Prelude (fold, map)

--------------------------------------------------------------------------------
import Web.PubSub.Command
import Web.PubSub.Control.Trans
import Web.PubSub.Type

--------------------------------------------------------------------------------
type Iterator m = Producer' Slot m ()

--------------------------------------------------------------------------------
data EventStore m
    = EventStore
      { storeSource   :: Callback -> Topic -> Iterator m
      , storeAddEvent :: forall e. Callback -> Topic -> Event e -> m ()
      }

--------------------------------------------------------------------------------
unverifiedSub :: Monad m => EventStore m -> Callback -> Topic -> m (Maybe Sub)
unverifiedSub store cb tp = fold go Nothing id (storeSource store cb tp)
  where
    go :: Maybe Sub -> Slot -> Maybe Sub
    go r (Slot _ _ ev _)
        = case ev of
              SubEvent sub -> Just sub
              SubVerified  -> Nothing
              _            -> r

--------------------------------------------------------------------------------
data VerifyState
    = VerifyNone
    | VerifyFound Sub
    | Verified Sub

verifiedSub :: Monad m => EventStore m -> Callback -> Topic -> m (Maybe Sub)
verifiedSub store cb tp = fold go VerifyNone done (storeSource store cb tp)
  where
    done (Verified s) = Just s
    done _            = Nothing

    go :: VerifyState -> Slot -> VerifyState
    go r (Slot _ _ ev _)
        = case ev of
              SubEvent sub
                  -> VerifyFound sub
              SubVerified
                  | VerifyFound sub <- r -> Verified sub
                  | otherwise            -> r
              SubVerifyFailed
                  -> VerifyNone
