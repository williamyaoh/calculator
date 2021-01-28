module Calculator.Components.Router where

import Prelude

import Data.Maybe ( Maybe(..), fromMaybe )
import Data.Either ( hush )

import Effect.Class ( class MonadEffect )

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Routing.Duplex ( parse )
import Routing.Hash ( getHash )

import Calculator.Routes ( Route(..), routeCodec )
import Calculator.Navigate ( class Navigate, navigate )

type State =
  { route :: Maybe Route
  }

data Query a = Navigate Route a

data Action
  = Initialize
  | GoTo Route

type Slots = ()

component :: forall i o m.
             MonadEffect m
          => Navigate m
          => H.Component HH.HTML Query i o m
component = H.mkComponent
  { initialState: const { route: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , initialize = Just Initialize
    }
  }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state = case state.route of
  Nothing -> HH.h1_ [ HH.text "Couldn't find that page." ]
  Just SelfIntro -> HH.h1_ [ HH.text "What's your name?" ]
  Just Calculator -> HH.h1_ [ HH.text "CALC-U-LATOR 3XXX" ]

handleQuery :: forall a o m. Query a -> H.HalogenM State Action Slots o m ( Maybe a )
handleQuery = case _ of
  Navigate dest a -> do
    here <- H.gets _.route
    when (here /= Just dest) $
      H.modify_ _ { route = Just dest }
    pure ( Just a )

handleAction :: forall o m.
                MonadEffect m
             => Navigate m
             => Action
             -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    initialRoute <- hush <<< ( parse routeCodec ) <$> H.liftEffect getHash
    navigate $ fromMaybe SelfIntro initialRoute
  GoTo dest -> do
    here <- H.gets _.route
    when ( here /= Just dest) $ navigate dest
