{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.Scotty
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.Scotty where

--------------------------------------------------------------------------------
import Data.Monoid ((<>))

--------------------------------------------------------------------------------
import Control.Monad.Trans
import Data.Semigroup.Foldable
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Network.HTTP.Types
import System.Random hiding (next)
import Web.Scotty

--------------------------------------------------------------------------------
import Web.PubSub.Command
import Web.PubSub.Control.Trans
import Web.PubSub.EventStore
import Web.PubSub.Hub
import Web.PubSub.Param
import Web.PubSub.Subscription
import Web.PubSub.Type
import Web.PubSub.Wreq

--------------------------------------------------------------------------------
scottyHub :: IO (Hub ActionM)
scottyHub
    = do g <- newStdGen
         setStdGen g
         return Hub
                { hubParamGet     = scottyParamGet
                , hubVerification = trans liftIO wreqVerification
                }

--------------------------------------------------------------------------------
generateChallenge :: ActionM String
generateChallenge = liftIO go where
  go = do g <- getStdGen
          let (n, g')   = randomR (8, 16) g
              challenge = take n $ randoms g'
          setStdGen g'

          return challenge

--------------------------------------------------------------------------------
scottyParamGet :: ParamGet ActionM
scottyParamGet
    = ParamGet
      { getRequiredParam = \k -> param $ fromStrict k
      , getOptionalParam = _optionalParam
      }

--------------------------------------------------------------------------------
_optionalParam :: Text -> ActionM (Maybe Text)
_optionalParam k
    = rescue (fmap Just $ param $ fromStrict k) (const $ return Nothing)

--------------------------------------------------------------------------------
makeScotty :: EventStore IO -> Hub ActionM -> ScottyM ()
makeScotty store h
    = post "/hub" $
          do mode <- param "hub.mode"
             case (mode :: Text) of
                 "subscribe"   -> subscribing store h Subscribing
                 "unsubscribe" -> subscribing store h Unsubscribing
                 _             -> next

--------------------------------------------------------------------------------
subscribing :: EventStore IO -> Hub ActionM -> Command Request -> ActionM ()
subscribing store h c
    = do res <- execute h c
         case res of
             Left (SubError es)
                 -> do status status400
                       text $ fromStrict $ foldedSubErrorText es
             Right ev@(SubEvent sub)
                 -> do liftIO $
                           storeAddEvent store
                           (subCallback sub) (subTopic sub) ev
                       status status202
