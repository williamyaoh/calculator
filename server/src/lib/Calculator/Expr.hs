{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calculator.Expr
  ( Expr(..)
  , toText, evaluate
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Text        ( Text, pack )

import Control.Applicative ( (<|>) )
import Control.Monad       ( unless )

data Expr
  = Plus Expr Expr
  | Minus Expr Expr
  | Divide Expr Expr
  | Multiply Expr Expr
  | NumberExpr Double
  deriving (Eq, Show)

instance FromJSON Expr where
  parseJSON obj = parseAtom obj <|> parseOperator obj

parseAtom :: Value -> Parser Expr
parseAtom = withObject "Expr[Atom]" $ \obj -> do
  type_ :: Text <- obj .: "type"
  unless (type_ == "atom") $
    fail "incorrect object type"
  NumberExpr <$> obj .: "value"

parseOperator :: Value -> Parser Expr
parseOperator = withObject "Expr[Operator]" $ \obj -> do
  type_ :: Text <- obj .: "type"
  name :: Text <- obj .: "name"
  unless (type_ == "operator") $
    fail "incorrect object type"
  op <- case name of
    "plus"     -> pure Plus
    "minus"    -> pure Minus
    "divide"   -> pure Divide
    "multiply" -> pure Multiply
    _otherwise -> fail "unknown operator name"
  op <$> obj .: "left" <*> obj .: "right"

toText :: Expr -> Text
toText (Plus left right)     = toText left <> "+" <> toText right
toText (Minus left right)    = toText left <> "-" <> toText right
toText (Divide left right)   = toText left <> "/" <> toText right
toText (Multiply left right) = toText left <> "*" <> toText right
toText (NumberExpr n)        = pack (show n)

evaluate :: Expr -> Double
evaluate (Plus left right)     = evaluate left + evaluate right
evaluate (Minus left right)    = evaluate left - evaluate right
evaluate (Divide left right)   = evaluate left / evaluate right
evaluate (Multiply left right) = evaluate left * evaluate right
evaluate (NumberExpr n)        = n
