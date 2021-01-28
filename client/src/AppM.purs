module AppM where

import Prelude

import Calculator.Navigate
import Calculator.Routes

import Effect.Class ( class MonadEffect, liftEffect )
import Effect.Aff ( Aff )
import Effect.Aff.Class ( class MonadAff )

import Routing.Hash ( setHash )
import Routing.Duplex ( print )

newtype AppM a = AppM ( Aff a )

runAppM :: AppM ~> Aff
runAppM ( AppM m ) = m

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print routeCodec
