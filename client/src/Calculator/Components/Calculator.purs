module Calculator.Components.Calculator where

import Prelude

import Data.Maybe ( Maybe(..) )
import Data.Either ( Either(..) )
import Control.Monad.Except ( runExcept )

import Effect.Class ( liftEffect )
import Effect.Aff.Class ( class MonadAff )

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource ( eventListenerEventSource )

import Web.HTML ( window )
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as Document
import Web.Storage.Storage as Storage
import Web.Event.Event ( preventDefault )
import Web.UIEvent.KeyboardEvent as KB
import Web.UIEvent.KeyboardEvent.EventTypes as KBE
import Web.Socket.WebSocket as WS
import Web.Socket.Event.EventTypes ( onMessage )
import Web.Socket.Event.MessageEvent as WSM

import Foreign as F

import Effect.Console ( log )

import Calculator.Routes ( Route(..) )
import Calculator.Navigate ( class Navigate, navigate )
import Calculator.LocalStorage ( localStorage )

type State =
  { calculation :: String
  , name        :: String
  }

data Action
  = Initialize
  | Keydown KB.KeyboardEvent
  | Keypress KB.KeyboardEvent
  | Message WSM.MessageEvent

type Slots = ()

component :: forall q i o m.
             MonadAff m
          => Navigate m
          => H.Component HH.HTML q i o m
component = H.mkComponent
  { initialState: const
    { calculation: ""
    , name: ""
    }
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }
  }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div_ [ HH.text $ show state ]

handleAction :: forall o m.
                MonadAff m
             => Navigate m
             => Action
             -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    initState
    initSocket
    subscribeEvents
  Keydown e ->
    -- This is here to intercept forward slash before it's handled
    -- by the browser.
    when (KB.key e == "/") do
      liftEffect $ preventDefault $ KB.toEvent e
  Keypress e -> do
    liftEffect $ preventDefault $ KB.toEvent e
    liftEffect $ log $ KB.key e
  Message m -> do
    liftEffect $ case runExcept (F.readString $ WSM.data_ m) of
      Left errs -> log (show errs)
      Right s -> log s

initState :: forall o m.
            MonadAff m
         => Navigate m
         => H.HalogenM State Action Slots o m Unit
initState = do
  mName <- liftEffect $ Storage.getItem "name" =<< localStorage
  case mName of
    Nothing -> navigate SelfIntro
    Just name -> H.modify_ _ { name = name }

initSocket :: forall o m.
              MonadAff m
           => H.HalogenM State Action Slots o m Unit
initSocket = do
  socket <- liftEffect $ WS.create "ws://localhost:8000/app/socket" []
  void $ H.subscribe $
    eventListenerEventSource
      onMessage
      (WS.toEventTarget socket)
      (map Message <<< WSM.fromEvent)

subscribeEvents :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
subscribeEvents = do
  document <- liftEffect $ Window.document =<< window
  void $ H.subscribe $
    eventListenerEventSource
    KBE.keydown
    (Document.toEventTarget document)
    (map Keydown <<< KB.fromEvent)
  void $ H.subscribe $
    eventListenerEventSource
    KBE.keyup
    (Document.toEventTarget document)
    (map Keypress <<< KB.fromEvent)
