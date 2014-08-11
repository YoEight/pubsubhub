module Web.PubSub.Validation where

import Network.URL

data Validation m
    = Validation
      { validateSubscription :: URL -> m Bool }
