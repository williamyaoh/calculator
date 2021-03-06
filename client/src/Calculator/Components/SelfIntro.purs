module Calculator.Components.SelfIntro where

import Prelude

import Data.Maybe ( Maybe(..) )

import Effect.Class ( class MonadEffect, liftEffect )

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Web.Event.Event ( Event, preventDefault )
import Web.Storage.Storage as Storage

import Calculator.Routes ( Route(..) )
import Calculator.Navigate ( class Navigate, navigate )
import Calculator.LocalStorage ( localStorage )

type State =
  { name :: String
  }

data Action
  = Initialize
  | UpdateName String
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
    , initialize = Just Initialize
    }
  }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ HP.id_ "self-intro" ]
    [ HH.h1_ [ HH.text "Hi there! What's your name?" ]
    , HH.form [ HE.onSubmit (Just <<< Submit) ]
      [ HH.input
        [ HP.type_ HP.InputText
        , HP.placeholder "Your name"
        , HP.required true
        , HE.onValueInput (Just <<< UpdateName)
        ]
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
  Initialize -> do
    mName <- liftEffect $ Storage.getItem "name" =<< localStorage
    case mName of
      Nothing -> pure unit
      Just _ -> navigate Calculator  -- if name already set, go directly to calculator
  UpdateName name ->
    H.modify_ _ { name = name }
  Submit e -> do
    -- Because we're only asking for a name and we don't care about
    -- doing any sort of authentication, it's okay to not do any sort
    -- of auth and just put the name in localStorage.
    liftEffect $ preventDefault e
    name <- H.gets _.name
    liftEffect $ Storage.setItem "name" name =<< localStorage
    navigate Calculator
