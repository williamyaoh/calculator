-- |
-- For convenience of implementation, we store the inputs on the
-- calculator itself as a sequence of tokens. But once we want to
-- evaluate them, we want to parse them into an expression tree,
-- which we'll eventually send to the backend to evaluate.

module Calculator.Expr
  ( Token(..), Expr(..)
  , displayTokens, tokensToExpr
  , expr, token, number
  )
where

import Prelude

import Data.Maybe ( Maybe(..) )
import Data.Foldable ( foldMap )
import Data.List ( List, fromFoldable )
import Data.Tuple
import Data.Array ( some, unsnoc, zip, length, (..) )
import Data.Int ( toNumber )
import Data.Either ( Either(..) )
import Data.Enum ( upFromIncluding )

import Text.Parsing.Parser ( Parser, ParseError, runParser )
import Text.Parsing.Parser.Pos ( Position(..) )
import Text.Parsing.Parser.Token as Token
import Text.Parsing.Parser.Expr ( Assoc(..), Operator(..), buildExprParser )

data Token
  = PlusToken
  | MinusToken
  | DivideToken
  | MultiplyToken
  | DigitToken Int

derive instance eqToken :: Eq Token

data Expr
  = Plus Expr Expr
  | Minus Expr Expr
  | Divide Expr Expr
  | Multiply Expr Expr
  | NumberExpr Number

-- We don't really need these show instances, get rid of them later

instance showToken :: Show Token where
  show PlusToken = "+"
  show MinusToken = "-"
  show DivideToken = "/"
  show MultiplyToken = "*"
  show (DigitToken i) = show i

instance showExpr :: Show Expr where
  show (Plus x y) = "(" <> show x <> "+" <> show y <> ")"
  show (Minus x y) = "(" <> show x <> "-" <> show y <> ")"
  show (Divide x y) = show x <> "/" <> show y
  show (Multiply x y) = show x <> "*" <> show y
  show (NumberExpr n) = show n

displayTokens :: Array Token -> String
displayTokens = foldMap display
  where
    display :: Token -> String
    display PlusToken = "+"
    display MinusToken = "-"
    display DivideToken = "/"
    display MultiplyToken = "*"
    display (DigitToken i) = show i

type ParseInput = List (Tuple Int Token)

tokensToExpr :: Array Token -> Either ParseError Expr
tokensToExpr tokens =
  let indexed = fromFoldable $ zip (1..length tokens) tokens
  in runParser indexed expr

expr :: Parser ParseInput Expr
expr = flip buildExprParser (map NumberExpr number)
  [ [ Infix (token DivideToken Divide) AssocRight
    , Infix (token MultiplyToken Multiply) AssocRight
    ]
  , [ Infix (token MinusToken Minus) AssocRight
    , Infix (token PlusToken Plus) AssocRight
    ]
  ]

token :: forall a. Token -> a -> Parser ParseInput a
token expected out = out <$ Token.when
  (\tup -> Position { column: fst tup, line: 1 })
  (\tup -> snd tup == expected)

number :: Parser ParseInput Number
number = pure <<< finalize =<< some digit
  where
    digit :: Parser _ Int
    digit = map (unwrap <<< snd) $ Token.when
      (\tup -> Position { column: fst tup, line: 1 })
      (\tup -> case snd tup of
          DigitToken _ -> true
          _otherwise   -> false)

    -- I'm not sure why purescript-parsing doesn't export a function that
    -- allows you to map a token to a Maybe a to allow you to select it.
    -- Instead we have to do this, which should always be safe because
    -- we only ever call it on tokens we've verified to be digits.
    unwrap :: Token -> Int
    unwrap (DigitToken i) = i
    unwrap _otherwise = 0

    finalize :: Array Int -> Number
    finalize arr = case unsnoc arr of
      Nothing ->
        0.0
      Just { init, last } ->
        toNumber last + (finalize init * 10.0)
