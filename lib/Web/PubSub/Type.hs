{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.Type
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.Type where

--------------------------------------------------------------------------------
import Data.Semigroup
import Data.Text (Text)
import Network.HTTP.Types
import Network.URL

--------------------------------------------------------------------------------
newtype Callback
    = Callback { unCallback :: URL }
    deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
newtype Topic
    = Topic { unTopic :: URL }
    deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
newtype LeaseSeconds
    = LeaseSeconds { unLeaseSeconds :: Int }
    deriving Show

--------------------------------------------------------------------------------
newtype Secret
    = Secret { unSecret :: Text }
    deriving Show

--------------------------------------------------------------------------------
data SubMode
    = Subscribe
    | Unsubscribe
    deriving Show

--------------------------------------------------------------------------------
data Sub
    = Sub
      { subCallback     :: !Callback
      , subTopic        :: !Topic
      , subMode         :: !SubMode
      , subLeaseSeconds :: !(Maybe LeaseSeconds)
      , subSecret       :: !(Maybe Secret)
      }
    deriving Show

--------------------------------------------------------------------------------
data SubInput
    = SubInput
      { subInCallback     :: !Text
      , subInTopic        :: !Text
      , subInMode         :: !Text
      , subInLeaseSeconds :: !(Maybe Text)
      , subInSecret       :: !(Maybe Text)
      }

--------------------------------------------------------------------------------
data PubSubError
   = PubSubError SomeError
   | PubSubBin PubSubError PubSubError

--------------------------------------------------------------------------------
data SomeError
    = forall e.
      SomeError
      { someError   :: e
      , errorText   :: e -> Text
      , errorStatus :: e -> Status
      }

--------------------------------------------------------------------------------
instance Semigroup PubSubError where
    (<>) = PubSubBin

--------------------------------------------------------------------------------
pubSubError :: SomeError -> PubSubError
pubSubError = PubSubError

--------------------------------------------------------------------------------
modeString :: SubMode -> String
modeString Subscribe   = "subscribe"
modeString Unsubscribe = "unsubscribe"

--------------------------------------------------------------------------------
pseFoldMap :: Semigroup m => (SomeError -> m) -> PubSubError -> m
pseFoldMap k (PubSubError e) = k e
pseFoldMap k (PubSubBin l r) = let !m = pseFoldMap k l <> pseFoldMap k r in m
