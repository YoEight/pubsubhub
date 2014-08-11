{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.Hub
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.Hub
    ( Hub(..)
    , execute
    ) where

--------------------------------------------------------------------------------
import Control.Monad

--------------------------------------------------------------------------------
import Data.Bifunctor
import Data.Time (UTCTime)
import Data.Bitraversable

--------------------------------------------------------------------------------
import Web.PubSub.Command
import Web.PubSub.EventStore
import Web.PubSub.Control.Trans
import Web.PubSub.Param
import Web.PubSub.Subscription
import Web.PubSub.Type
import Web.PubSub.Verification

--------------------------------------------------------------------------------
data Hub m
    = Hub
      { hubParamGet     :: ParamGet m
      , hubVerification :: Verification m
      }

--------------------------------------------------------------------------------
instance Trans Hub where
    trans k h
        = Hub
          { hubParamGet     = trans k $ hubParamGet h
          , hubVerification = trans k $ hubVerification h
          }

--------------------------------------------------------------------------------
execute :: Monad m => Hub m -> Command a -> m (Either (Error a) (Event a))
execute h c
    = case c of
        Subscribing      -> subscribing h
        VerifySub sub ch -> verifyingSub h sub ch

--------------------------------------------------------------------------------
subscribing :: Monad m => Hub m -> m (Either (Error Request) (Event Request))
subscribing h
    = do cb <- getRequiredParam pg "hub.callback"
         tp <- getRequiredParam pg "hub.topic"
         md <- getRequiredParam pg "hub.mode"
         ls <- getOptionalParam pg "hub.lease_seconds"
         sc <- getOptionalParam pg "hub.secret"

         let input = SubInput
                     { subInCallback     = cb
                     , subInTopic        = tp
                     , subInMode         = md
                     , subInLeaseSeconds = ls
                     , subInSecret       = sc
                     }

         return $ bimap SubError SubEvent (runSub input)
  where
    pg = hubParamGet h

--------------------------------------------------------------------------------
verifyingSub :: Monad m
             => Hub m
             -> Sub
             -> String
             -> m (Either (Error Verify) (Event Verify))
verifyingSub h sub challenge
    = do valid <- (verifySubscription vr) challenge url

         return $ Right $
             if valid
             then SubVerified
             else SubVerifyFailed
  where
    vr  = hubVerification h
    url = makeVerificationURL challenge sub
