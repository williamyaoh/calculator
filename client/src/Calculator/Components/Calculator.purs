module Calculator.Components.Calculator where

import Prelude

import Math as M

import Data.Maybe ( Maybe(..), maybe )
import Data.Array ( snoc, unsnoc )
import Data.Int ( toNumber, round )
import Data.Enum ( enumFromTo )
import Data.Foldable ( foldMap )

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Web.UIEvent.MouseEvent ( MouseEvent )

import Calculator.Expr

type State =
  { tokens :: Array Token
  }

data Action
  = Initialize
  | NewToken Token
  | Delete
  | Evaluate

type Slots = ()

component :: forall q i o m. H.Component HH.HTML q i o m
component = H.mkComponent
  { initialState: const
    { tokens: []
    }
  , render
  , eval: H.mkEval $ H.defaultEval
    { initialize = Just Initialize
    , handleAction = handleAction
    }
  }

button :: forall m.
          String
       -> Boolean
       -> (MouseEvent -> Maybe Action)
       -> H.ComponentHTML Action Slots m
button label pressed onPress =
  HH.button [ HE.onClick onPress ] [ HH.text label ]

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div_
    [ HH.div_ [ HH.text $ displayTokens state.tokens ]
    , HH.div_ $
        flip map (enumFromTo 0 9) (\i ->
          button (show i) false (const $ Just $ NewToken $ DigitToken i))
        <> [ button "+" false (const $ Just $ NewToken PlusToken)
           , button "-" false (const $ Just $ NewToken MinusToken)
           , button "×" false (const $ Just $ NewToken MultiplyToken)
           , button "÷" false (const $ Just $ NewToken DivideToken)
           , button "⌫" false (const $ Just $ Delete)
           , button "=" false (const $ Just $ Evaluate)
           ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> pure unit
  NewToken tok -> H.modify_ \st ->
    st { tokens = snoc st.tokens tok }
  Delete -> H.modify_ \st ->
    st { tokens = maybe st.tokens _.init $ unsnoc st.tokens }
  Evaluate -> pure unit
