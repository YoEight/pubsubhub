--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.Wreq
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.Wreq (wreqVerification) where

--------------------------------------------------------------------------------
import Data.Bool
import Data.ByteString.Lazy.Char8 (pack)

--------------------------------------------------------------------------------
import Control.Lens ((^.))
import Control.Monad.Trans
import Network.URL
import Network.Wreq

--------------------------------------------------------------------------------
import Web.PubSub.Verification

--------------------------------------------------------------------------------
wreqVerification :: Verification IO
wreqVerification = Verification wreqImpl

--------------------------------------------------------------------------------
wreqImpl :: String -> URL -> IO Bool
wreqImpl challenge url
    = do resp <- get (exportURL url)
         let res  = resp ^. responseBody == pack challenge
             code = resp ^. responseStatus.statusCode
             ok   = 200 <= code && code < 300
         return $ bool False res ok
