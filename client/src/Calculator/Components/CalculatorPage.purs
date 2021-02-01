module Calculator.Components.CalculatorPage where

import Prelude

import Data.Array as Array
import Data.Maybe ( Maybe(..), fromMaybe )
import Data.Either ( Either(..) )
import Data.Foldable ( traverse_ )
import Data.Symbol ( SProxy(..) )
import Data.Argonaut.Parser as JSON
import Data.Codec as C

import Control.Monad.Except ( runExcept )

import Effect.Class ( liftEffect )
import Effect.Aff.Class ( class MonadAff )

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core ( ClassName(..) )
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource ( eventListenerEventSource )

import Web.Storage.Storage as Storage
import Web.Socket.WebSocket as WS
import Web.Socket.Event.EventTypes ( onMessage )
import Web.Socket.Event.MessageEvent as WSM

import Foreign as F

import Effect.Console ( log )

import Calculator.Routes ( Route(..) )
import Calculator.Calculation ( Calculation, calculationCodec )
import Calculator.Expr ( Expr )
import Calculator.API ( evaluate, calculations )
import Calculator.WebSocket ( calculationSocket )
import Calculator.Navigate ( class Navigate, navigate )
import Calculator.LocalStorage ( localStorage )
import Calculator.Components.Calculator as Calculator

type State =
  { conn         :: Maybe WS.WebSocket
  , name         :: String
  , calculations :: Array Calculation
  }

data Action
  = Initialize
  | Finalize
  | Message WSM.MessageEvent
  | EvalRequest Expr

type Slots =
  ( calculator :: H.Slot Calculator.Query Calculator.Output Unit )

component :: forall q i o m.
             MonadAff m
          => Navigate m
          => H.Component HH.HTML q i o m
component = H.mkComponent
  { initialState: const
    { conn: Nothing
    , name: ""
    , calculations: []
    }
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize = Just Finalize
    }
  }

-- Funnily enough, the calculation history here is... an actual table of data!
-- Which makes using HTML tables here justified.

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ HP.id_ "calculator-page" ]
    [ HH.main [ HP.id_ "calculator" ]
      [ HH.h2_ [ HH.text $ "Hi, " <> state.name <> "!" ]
      , HH.slot (SProxy :: _ "calculator") unit Calculator.component unit (Just <<< mapOutput)
      ]
    , HH.aside [ HP.id_ "calculation-history" ] $
      [ HH.h2_ [ HH.text "Previous calculations" ]
      , HH.table_ $ map displayCalculation state.calculations
      ]
    ]

  where mapOutput :: Calculator.Output -> Action
        mapOutput (Calculator.EvalRequest expr) = EvalRequest expr

handleAction :: forall o m.
                MonadAff m
             => Navigate m
             => Action
             -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    checkName
    initCalculations
    initSocket
  Finalize -> do
    mConn <- H.gets _.conn
    liftEffect $ traverse_ WS.close mConn
  Message m ->
    case runExcept (F.readString $ WSM.data_ m) of
      Left errs -> liftEffect $ log $ show errs
      Right s -> case JSON.jsonParser s of
        Left err -> liftEffect $ log err
        Right json -> case C.decode calculationCodec json of
          Left err -> liftEffect $ log $ show err
          Right calc -> do
            calcs <- H.gets _.calculations
            H.modify_ _ { calculations = Array.take 10 $ Array.cons calc calcs }
  EvalRequest expr -> do
    name <- H.gets _.name
    mCalc <- evaluate { user: name, expr }
    case mCalc of
      Nothing -> liftEffect $ log "something went wrong with evaluating"
      Just calc ->
        void $ H.query (SProxy :: _ "calculator") unit (H.tell $ Calculator.Result calc.result)

checkName :: forall o m.
             MonadAff m
          => Navigate m
          => H.HalogenM State Action Slots o m Unit
checkName = do
  mName <- liftEffect $ Storage.getItem "name" =<< localStorage
  case mName of
    Nothing -> navigate SelfIntro
    Just name -> H.modify_ _ { name = name }

initCalculations :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
initCalculations = do
  calcs <- calculations
  H.modify_ _ { calculations = Array.take 10 $ fromMaybe [] calcs }

initSocket :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
initSocket = do
  socket <- calculationSocket
  void $ H.subscribe $
    eventListenerEventSource
      onMessage
      (WS.toEventTarget socket)
      (map Message <<< WSM.fromEvent)
  H.modify_ _ { conn = Just socket }

displayCalculation :: forall a slots m. Calculation -> H.ComponentHTML a slots m
displayCalculation calc =
  HH.tr [ HP.class_ (ClassName "calculation") ]
    [ HH.td [ HP.class_ (ClassName "calculation-user") ] [ HH.text calc.user ]
    , HH.td [ HP.class_ (ClassName "calculation-text") ] [ HH.text calc.text ]
    , HH.td [ HP.class_ (ClassName "calculation-equals") ] [ HH.text "=" ]
    , HH.td [ HP.class_ (ClassName "calculation-result") ] [ HH.text $ show calc.result ]
    ]
