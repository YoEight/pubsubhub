--------------------------------------------------------------------------------
-- |
-- Module : Main
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
import Web.Scotty

--------------------------------------------------------------------------------
import Web.PubSub.InMemoryEventStore
import Web.PubSub.Scotty

--------------------------------------------------------------------------------
main :: IO ()
main = do hub   <- scottyHub
          store <- newInMemoryEventStore
          scotty 300 $ makeScotty store hub
