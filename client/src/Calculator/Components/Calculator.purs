module Calculator.Components.Calculator where

import Prelude

import Math as M

import Effect.Class ( liftEffect )
import Effect.Aff.Class ( class MonadAff )

import Data.Maybe ( Maybe(..), maybe )
import Data.Either ( Either(..) )
import Data.Array ( snoc, unsnoc, null )
import Data.Int ( toNumber, round )
import Data.Enum ( enumFromTo )
import Data.Foldable ( foldMap, traverse_ )

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource ( eventListenerEventSource )

import Web.Event.Event ( preventDefault )
import Web.UIEvent.MouseEvent ( MouseEvent )
import Web.UIEvent.KeyboardEvent as KB
import Web.UIEvent.KeyboardEvent.EventTypes as KBE
import Web.HTML ( window )
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as Document

import Calculator.Expr

import Effect.Console ( log )
import Data.Argonaut.Core ( stringifyWithIndent )


type State =
  { tokens :: Array Token
  }

data Action
  = Initialize
  | NewToken Token
  | Delete
  | Evaluate
  | Keydown KB.KeyboardEvent
  | Keypress KB.KeyboardEvent

newtype Output = EvalRequest Expr

type Slots = ()

component :: forall q i m. MonadAff m => H.Component HH.HTML q i Output m
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

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> subscribeEvents
  NewToken tok -> H.modify_ \st ->
    st { tokens = snoc st.tokens tok }
  Delete -> H.modify_ \st ->
    st { tokens = maybe st.tokens _.init $ unsnoc st.tokens }
  Evaluate -> do
    tok <- H.gets _.tokens
    unless (null tok) do
      case tokensToExpr tok of
        Left err -> liftEffect $ log $ show err
        Right expr -> do
          liftEffect $ log $ stringifyWithIndent 2 $ exprToJSON expr
          H.raise (EvalRequest expr)
      H.modify_ _ { tokens = [] }
  Keydown e ->
    -- To intercept forward slash before it's handled by the browser.
    when (KB.key e == "/") do
      liftEffect $ preventDefault $ KB.toEvent e
  Keypress e -> do
    liftEffect $ preventDefault $ KB.toEvent e
    traverse_ handleAction (keypressToAction $ KB.key e)

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
