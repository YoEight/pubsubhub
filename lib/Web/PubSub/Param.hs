--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.Param
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.Param where

--------------------------------------------------------------------------------
import Data.Text (Text)

--------------------------------------------------------------------------------
import Web.PubSub.Control.Trans

--------------------------------------------------------------------------------
data ParamGet m
    = ParamGet
      { getRequiredParam :: Text -> m Text
      , getOptionalParam :: Text -> m (Maybe Text)
      }

--------------------------------------------------------------------------------
instance Trans ParamGet where
    trans k p
        = ParamGet
          { getRequiredParam = \t -> k $ getRequiredParam p t
          , getOptionalParam = \t -> k $ getOptionalParam p t
          }
