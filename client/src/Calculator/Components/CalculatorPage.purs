module Calculator.Components.CalculatorPage where

import Prelude

import Data.Maybe ( Maybe(..) )
import Data.Either ( Either(..) )
import Data.Foldable ( traverse_ )
import Data.Symbol ( SProxy(..) )

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
import Calculator.Expr ( Expr )
import Calculator.API ( evaluate )
import Calculator.Navigate ( class Navigate, navigate )
import Calculator.LocalStorage ( localStorage )
import Calculator.Components.Calculator as Calculator

type State =
  { calculation :: String
  , name        :: String
  , conn        :: Maybe WS.WebSocket
  }

data Action
  = Initialize
  | Finalize
  | Keydown KB.KeyboardEvent
  | Keypress KB.KeyboardEvent
  | Message WSM.MessageEvent
  | EvalRequest Expr

type Slots =
  ( calculator :: forall query. H.Slot query Calculator.Output Unit )

component :: forall q i o m.
             MonadAff m
          => Navigate m
          => H.Component HH.HTML q i o m
component = H.mkComponent
  { initialState: const
    { calculation: ""
    , name: ""
    , conn: Nothing
    }
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize = Just Finalize
    }
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.slot (SProxy :: _ "calculator") unit Calculator.component unit (Just <<< mapQuery)
  where mapQuery :: Calculator.Output -> Action
        mapQuery (Calculator.EvalRequest expr) = EvalRequest expr

handleAction :: forall o m.
                MonadAff m
             => Navigate m
             => Action
             -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    checkName
    initSocket
    subscribeEvents
  Finalize -> do
    mConn <- H.gets _.conn
    liftEffect $ traverse_ WS.close mConn
  Keydown e ->
    -- To intercept forward slash before it's handled by the browser.
    when (KB.key e == "/") do
      liftEffect $ preventDefault $ KB.toEvent e
  Keypress e -> do
    liftEffect $ preventDefault $ KB.toEvent e
    liftEffect $ log $ KB.key e
  Message m -> do
    liftEffect $ case runExcept (F.readString $ WSM.data_ m) of
      Left errs -> log (show errs)
      Right s -> log s
  EvalRequest expr -> do
    name <- H.gets _.name
    mCalc <- evaluate { user: name, expr }
    case mCalc of
      Nothing -> liftEffect $ log "something went wrong with evaluating"
      Just calc -> liftEffect $ log $ show calc

checkName :: forall o m.
             MonadAff m
          => Navigate m
          => H.HalogenM State Action Slots o m Unit
checkName = do
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
  H.modify_ _ { conn = Just socket }

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

-- So if we look at the design of this calculator thing, we've got a few
-- components that we need to put together:
--
-- * The internal design of our expressions -- clearly we don't want
--   to just store them as strings. probably we want some sort of tree structure
--   in fact, we want both the individual tokens to list + the tree structure
-- * The display of the calculator itself, which shows the current list of tokens
-- * The buttons, which need to respond to both clicking them and to hitting
--   the keyboard, and which add a token to our thing... or update the current one
-- * Algorithm to take our list of tokens and turn them into RPN/tree, which
--   is basically just the shunting yard algorithm
-- * A display of the previous calculations, which is a simple string display...
--   Except for one extra thing, which is showing the name + a colored bar
--     and we also want to align the results properly
-- * A "hashing" function from names to colors

-- Hook up the keyboard events to the calculator itself, so that it actually
--  does stuff...
