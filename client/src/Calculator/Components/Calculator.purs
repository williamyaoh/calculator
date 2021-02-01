module Calculator.Components.Calculator where

import Prelude

import Effect.Class ( liftEffect )
import Effect.Aff.Class ( class MonadAff )

import Data.Maybe ( Maybe(..), maybe )
import Data.Either ( Either(..) )
import Data.Array ( snoc, unsnoc, null )
import Data.Enum ( enumFromTo )
import Data.Foldable ( traverse_ )

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core ( ClassName(..) )
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource ( eventListenerEventSource )

import Web.Event.Event ( preventDefault )
import Web.UIEvent.MouseEvent ( MouseEvent )
import Web.UIEvent.KeyboardEvent as KB
import Web.UIEvent.KeyboardEvent.EventTypes as KBE
import Web.HTML ( window )
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as Document

import Calculator.Expr ( Expr, Token(..), displayTokens, tokensToExpr )

data Display
  = DisplayResult Number
  | DisplayError
  | DisplayTokens

type State =
  { tokens :: Array Token
  , display :: Display
  }

data Action
  = Initialize
  | NewToken Token
  | Delete
  | Evaluate
  | Keydown KB.KeyboardEvent
  | Keypress KB.KeyboardEvent

data Query a
  = Result Number a
  | Error a

newtype Output = EvalRequest Expr

type Slots = ()

component :: forall i m. MonadAff m => H.Component HH.HTML Query i Output m
component = H.mkComponent
  { initialState: const
    { tokens: []
    , display: DisplayTokens
    }
  , render
  , eval: H.mkEval $ H.defaultEval
    { initialize = Just Initialize
    , handleAction = handleAction
    , handleQuery = handleQuery
    }
  }

button :: forall m.
          String
       -> (MouseEvent -> Maybe Action)
       -> H.ComponentHTML Action Slots m
button label onPress =
  HH.td_ [ HH.button [ HE.onClick onPress ] [ HH.text label ] ]

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ HP.id_ "calculator-body" ]
    [ HH.div [ HP.class_ (ClassName "display") ]
      [ HH.text $ case state.display of
          DisplayTokens -> displayTokens state.tokens
          DisplayError -> "error"
          DisplayResult n -> show n
      ]
    , HH.div [ HP.class_ (ClassName "button-grid") ] $
      [ HH.table_
        [ HH.tr_ $ digitButtonsFromTo 7 9 <>
                     [ button "+" (const $ Just $ NewToken PlusToken) ]
        , HH.tr_ $ digitButtonsFromTo 4 6 <>
                     [ button "-" (const $ Just $ NewToken MinusToken) ]
        , HH.tr_ $ digitButtonsFromTo 1 3 <>
                     [ button "×" (const $ Just $ NewToken MultiplyToken) ]
        , HH.tr_
          [ button "0" (const $ Just $ NewToken $ DigitToken 0)
          , button "=" (const $ Just $ Evaluate)
          , button "⌫" (const $ Just $ Delete)
          , button "÷" (const $ Just $ NewToken DivideToken)
          ]
        ]
      ]
    ]
  where
    digitButtonsFromTo :: Int -> Int -> Array (H.ComponentHTML Action Slots m)
    digitButtonsFromTo from to =
      flip map (enumFromTo from to) (\i ->
        button (show i) (const $ Just $ NewToken $ DigitToken i))

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> subscribeEvents
  NewToken tok -> H.modify_ \st ->
    st { tokens = snoc st.tokens tok, display = DisplayTokens }
  Delete -> H.modify_ \st ->
    st { tokens = maybe st.tokens _.init $ unsnoc st.tokens }
  Evaluate -> do
    tok <- H.gets _.tokens
    unless (null tok) do
      case tokensToExpr tok of
        Left _err -> H.modify_ _ { tokens = [], display = DisplayError }
        Right expr -> do
          H.modify_ _ { tokens = [] }
          H.raise (EvalRequest expr)
  Keydown e ->
    -- To intercept forward slash before it's handled by the browser.
    when (KB.key e == "/") do
      liftEffect $ preventDefault $ KB.toEvent e
  Keypress e -> do
    liftEffect $ preventDefault $ KB.toEvent e
    traverse_ handleAction (keypressToAction $ KB.key e)

handleQuery :: forall m a. Query a -> H.HalogenM State Action Slots Output m (Maybe a)
handleQuery = case _ of
  Result n a -> do
    H.modify_ _ { display = DisplayResult n }
    pure $ Just a
  Error a -> do
    H.modify_ _ { display = DisplayError }
    pure $ Just a

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

keypressToAction :: String -> Maybe Action
keypressToAction = case _ of
  "0" -> Just $ NewToken $ DigitToken 0
  "1" -> Just $ NewToken $ DigitToken 1
  "2" -> Just $ NewToken $ DigitToken 2
  "3" -> Just $ NewToken $ DigitToken 3
  "4" -> Just $ NewToken $ DigitToken 4
  "5" -> Just $ NewToken $ DigitToken 5
  "6" -> Just $ NewToken $ DigitToken 6
  "7" -> Just $ NewToken $ DigitToken 7
  "8" -> Just $ NewToken $ DigitToken 8
  "9" -> Just $ NewToken $ DigitToken 9
  "+" -> Just $ NewToken $ PlusToken
  "-" -> Just $ NewToken $ MinusToken
  "/" -> Just $ NewToken $ DivideToken
  "*" -> Just $ NewToken $ MultiplyToken
  "Backspace" -> Just $ Delete
  "Enter" -> Just $ Evaluate
  _otherwise -> Nothing
