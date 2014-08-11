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
import Web.PubSub.InMemoryEventStore
import Web.PubSub.Terminal

--------------------------------------------------------------------------------
main :: IO ()
main = do store <- newInMemoryEventStore
          hub   <- terminalHub
          terminalApp store hub
