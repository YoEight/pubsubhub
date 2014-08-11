--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.Hub.Scotty
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.Hub.Scotty where

--------------------------------------------------------------------------------
import Control.Monad.Trans
import Web.Scotty

--------------------------------------------------------------------------------
import Web.PubSub.Hub.Type
import Web.PubSub.Param
import Web.PubSub.Param.Wreq

--------------------------------------------------------------------------------
scottyHub :: Hub ActionM
scottyHub
    = Hub
      { hubParamGet     = scottyParamGet
      , hubVerification = liftP liftIO wreqVerification
      }

--------------------------------------------------------------------------------
scottyParamGet :: ParamGet ActionM
scottyParamGet
    = ParamGet
      { getRequiredParam = _requiredParam
      , getOptionalParam = _optionalParam
      }

--------------------------------------------------------------------------------
_requiredParam :: Text -> ActionM Text
_requiredParam p = param p

--------------------------------------------------------------------------------
_optionalParam :: Text -> ActionM (Maybe Text)
_optionalParam p = rescue (fmap Just $ param p) (const $ return Nothing)
