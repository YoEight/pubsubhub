{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.InMemoryEventStore
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.InMemoryEventStore where

--------------------------------------------------------------------------------
import           Data.Foldable (traverse_)
import           Data.IORef
import qualified Data.Map      as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq

--------------------------------------------------------------------------------
import Control.Monad.Trans
import Data.Time
import Pipes

--------------------------------------------------------------------------------
import Web.PubSub.Command
import Web.PubSub.EventStore
import Web.PubSub.Type

--------------------------------------------------------------------------------
type Key = (Callback, Topic)

--------------------------------------------------------------------------------
type Database = (IORef (Map.Map Key (Seq.Seq Slot)))

--------------------------------------------------------------------------------
newInMemoryEventStore :: IO (EventStore IO)
newInMemoryEventStore
    = do database <- newIORef Map.empty
         let store = EventStore
                   { storeSource   = _source database
                   , storeAddEvent = _addEvent database
                   }

         return store

--------------------------------------------------------------------------------
_addEvent :: Database -> Callback -> Topic -> Event e -> IO ()
_addEvent ref cb tp ev
    = liftIO $
      do db   <- readIORef ref
         date <- getCurrentTime
         let key  = (cb, tp)
             slot = Slot
                    { slotCallback = cb
                    , slotTopic    = tp
                    , slotEvent    = ev
                    , slotDate     = date
                    }

             alterK Nothing   = Just (Seq.singleton slot)
             alterK (Just as) = Just (as Seq.|> slot)

             db' = Map.alter alterK key db

         writeIORef ref db'

--------------------------------------------------------------------------------
_source :: Database -> Callback -> Topic -> Iterator IO
_source ref cb tp
    = do db  <- liftIO $ readIORef ref
         let key = (cb, tp)
             evs = fromMaybe Seq.empty $ Map.lookup key db
         each evs
