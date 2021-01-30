module Calculator.LocalStorage where

import Prelude

import Effect.Class ( class MonadEffect, liftEffect )

import Web.HTML ( window )
import Web.HTML.Window as Window
import Web.Storage.Storage ( Storage )

-- |
-- Domain-local key store.
localStorage :: forall m. MonadEffect m => m Storage
localStorage = liftEffect $ window >>= Window.localStorage
