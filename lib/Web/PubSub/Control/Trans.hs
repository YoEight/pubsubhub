{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.Control.Trans
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.Control.Trans where

--------------------------------------------------------------------------------
class Trans f where
    trans :: (forall a. m a -> n a) -> f m -> f n
