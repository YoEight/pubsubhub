--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.Verification
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.Verification where

--------------------------------------------------------------------------------
import Data.Text (Text)
import Network.URL

--------------------------------------------------------------------------------
import Web.PubSub.Control.Trans

--------------------------------------------------------------------------------
newtype Verification m
    = Verification { verifySubscription :: String -> URL -> m Bool }

--------------------------------------------------------------------------------
instance Trans Verification where
    trans f (Verification k) = Verification $ \s u -> f $ k s u
