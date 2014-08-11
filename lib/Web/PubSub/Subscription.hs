{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.Subscription
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.Subscription
    ( SubError(..)
    , runSub
    , makeVerificationURL
    , foldedSubErrorText
    , validateCallback
    , validateTopic
    , runValidation
    ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Data.Traversable

--------------------------------------------------------------------------------
import           Control.Lens.Getter (view)
import           Control.Lens.Review ((#))
import           Data.Attoparsec.Text
import           Data.List.NonEmpty
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Text (Text, append)
import qualified Data.Text as T
import           Data.Validation
import           Network.HTTP.Types
import           Network.URL

--------------------------------------------------------------------------------
import Web.PubSub.Type

--------------------------------------------------------------------------------
type Subscription a = AccValidation (NonEmpty SubError) a

--------------------------------------------------------------------------------
data SubError
    = UnknownMode Text Text
    | InvalidUrl Text Text
    | InvalidValue Text Text
    | ExceedingLimit Text Int

--------------------------------------------------------------------------------
subError :: SubError -> NonEmpty SubError
subError e = e :| []

--------------------------------------------------------------------------------
runSub :: SubInput -> Either (NonEmpty SubError) Sub
runSub i = runValidation $ validateSub i

--------------------------------------------------------------------------------
runValidation :: AccValidation (NonEmpty SubError) a
              -> Either (NonEmpty SubError) a
runValidation i = view isoAccValidationEither i

--------------------------------------------------------------------------------
foldedSubErrorText :: Foldable1 f => f SubError -> Text
foldedSubErrorText = foldMap1 $ \e -> subErrorText e <> "\n"

--------------------------------------------------------------------------------
subErrorText :: SubError -> Text
subErrorText e
    = case e of
        UnknownMode key m ->
            key `append` ": unknown mode '" `append` m `append` "'"
        InvalidUrl key u ->
            key `append` ": invalid url '" `append` u `append` "'"
        InvalidValue key v ->
            key `append` ": invalid value '" `append` v `append` "'"
        ExceedingLimit key l ->
            let l' = T.pack $ show l in
            key `append` ": length exceeds " `append` l' `append` " bytes"

--------------------------------------------------------------------------------
validateSub :: SubInput -> Subscription Sub
validateSub input
    = Sub                     <$>
      validateCallback cb     <*>
      validateTopic top       <*>
      validateMode mode       <*>
      validateLeaseSeconds ls <*>
      validateSecret sec
  where
    cb   = subInCallback input
    top  = subInTopic input
    mode = subInMode input
    ls   = subInLeaseSeconds input
    sec  = subInSecret input

--------------------------------------------------------------------------------
validateUrl :: Text -> Text -> Subscription URL
validateUrl key urlTxt
    = case importURL $ T.unpack urlTxt of
        Nothing -> _Failure # subError (InvalidUrl key urlTxt)
        Just u
            | Absolute h <- url_type u,
              HTTP _     <- protocol h
                -> _Success # u
            | otherwise -> _Failure # subError (InvalidUrl key urlTxt)

--------------------------------------------------------------------------------
validateCallback :: Text -> Subscription Callback
validateCallback url = fmap Callback $ validateUrl "hub.callback" url

--------------------------------------------------------------------------------
validateTopic :: Text -> Subscription Topic
validateTopic url = fmap Topic $ validateUrl "hub.topic" url

--------------------------------------------------------------------------------
validateMode :: Text -> Subscription SubMode
validateMode "subscribe"   = _Success # Subscribe
validateMode "unsubscribe" = _Success # Unsubscribe
validateMode m             = _Failure # subError (UnknownMode key m)
  where
    key = "hub.mode"

--------------------------------------------------------------------------------
validateLeaseSeconds :: Maybe Text -> Subscription (Maybe LeaseSeconds)
validateLeaseSeconds mi
    = traverse validatingLeaseSeconds mi

--------------------------------------------------------------------------------
validatingLeaseSeconds :: Text -> Subscription LeaseSeconds
validatingLeaseSeconds txt
    = case parseOnly (decimal <* endOfInput) txt of
        Left e  -> _Failure # subError (InvalidValue key $ T.pack e)
        Right i -> _Success # LeaseSeconds i
  where
    key = "hub.lease_seconds"

--------------------------------------------------------------------------------
validateSecret :: Maybe Text -> Subscription (Maybe Secret)
validateSecret ms =
    traverse validatingSecret ms

--------------------------------------------------------------------------------
validatingSecret :: Text -> Subscription Secret
validatingSecret s
    | T.length s < 200 = _Success # Secret s
    | otherwise        = _Failure # subError (ExceedingLimit key 200)
  where
    key = "hub.secret"

--------------------------------------------------------------------------------
defLeaseSeconds :: Int
defLeaseSeconds = 5 * 60 -- 5 minutes

--------------------------------------------------------------------------------
makeVerificationURL :: String -> Sub -> URL
makeVerificationURL challenge req
    = urlAddParams params callback
  where
    callback     = unCallback $ subCallback req
    mode         = subMode req
    topic        = unTopic $ subTopic req
    ls           = subLeaseSeconds req
    leaseSeconds = maybe (show defLeaseSeconds) (show . unLeaseSeconds) ls
    params       = [ ("hub.mode", modeString mode)
                   , ("hub.topic", exportURL topic)
                   , ("hub.challenge", challenge)
                   , ("hub.lease_seconds", leaseSeconds)
                   ]

--------------------------------------------------------------------------------
urlAddParams :: [(String, String)] -> URL -> URL
urlAddParams xs url
    = foldl (add_param) url xs
