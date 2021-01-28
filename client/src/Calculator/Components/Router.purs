module Calculator.Components.Router where

import Prelude

import Data.Maybe ( Maybe(..), fromMaybe )
import Data.Either ( hush )
import Data.Symbol ( SProxy(..) )

import Effect.Class ( class MonadEffect )

import Halogen as H
import Halogen.HTML as HH

import Routing.Duplex ( parse )
import Routing.Hash ( getHash )

import Calculator.Routes ( Route(..), routeCodec )
import Calculator.Navigate ( class Navigate, navigate )
import Calculator.Components.OpaqueSlot ( OpaqueSlot )
import Calculator.Components.SelfIntro as SelfIntro

type State =
  { route :: Maybe Route
  }

data Query a = Navigate Route a

data Action
  = Initialize
  | GoTo Route

type Slots =
  ( selfintro :: OpaqueSlot Unit
  )

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

render :: forall m.
          MonadEffect m
       => Navigate m
       => State
       -> H.ComponentHTML Action Slots m
render state = case state.route of
  Nothing -> HH.h1_ [ HH.text "Couldn't find that page." ]
  Just SelfIntro ->
    HH.slot (SProxy :: _ "selfintro") unit SelfIntro.component unit absurd
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
    -- If we can't parse the route into something we expect, just go to
    -- the self-intro page by default
    navigate $ fromMaybe SelfIntro initialRoute
  GoTo dest -> do
    here <- H.gets _.route
    when ( here /= Just dest) $ navigate dest
