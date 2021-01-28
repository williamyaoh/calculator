module Calculator.Components.SelfIntro where

import Prelude

import Data.Maybe ( Maybe(..) )

import Effect.Class ( class MonadEffect, liftEffect )

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Web.Event.Event ( Event, preventDefault )

import Calculator.Routes ( Route(..) )
import Calculator.Navigate ( class Navigate, navigate )
import Calculator.Cookies

type State =
  { name :: String
  }

data Action
  = UpdateName String
  | Submit Event

type Slots = ()

component :: forall q i o m.
             MonadEffect m
          => Navigate m
          => H.Component HH.HTML q i o m
component = H.mkComponent
  { initialState: const { name: "" }
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    }
  }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div_
    [ HH.text state.name
    , HH.form [ HE.onSubmit (Just <<< Submit) ]
      [ HH.input [ HP.type_ HP.InputText ]
      , button "To the calculator!"
      ]
    ]

button :: forall m. String -> H.ComponentHTML Action Slots m
button label =
  HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text label ]

handleAction :: forall o m.
                MonadEffect m
             => Navigate m
             => Action
             -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  UpdateName name ->
    H.modify_ _ { name = name }
  Submit e -> do
    liftEffect $ preventDefault e
    name <- H.gets _.name
    liftEffect $
      setCookie (Cookie { key: "name", value: name }) defaultCookieOpts
    navigate Calculator
