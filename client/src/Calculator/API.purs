module Calculator.API where

import Prelude

import Data.Argonaut.Core as JSON
import Data.Maybe ( Maybe(..) )
import Data.Either ( hush )
import Data.Codec.Argonaut as CA

import Effect.Aff.Class ( class MonadAff, liftAff )

import Affjax as AJAX
import Affjax.ResponseFormat ( json )
import Affjax.RequestBody as AJAXB

import Foreign.Object as Object

import Env ( HTTPMethod(..), env )

import Calculator.Expr ( Expr, exprToJSON )
import Calculator.Calculation ( Calculation, calculationCodec )

type EvalRequest = { user :: String, expr :: Expr }

-- |
-- Convert an API URL described as just the section after the domain,
-- into an absolute URL.
fromSegment :: String -> String
fromSegment api =
  methodToString env.httpMethod <> "://" <> env.baseURL <> api
  where
    methodToString :: HTTPMethod -> String
    methodToString HTTP = "http"
    methodToString HTTPS = "https"

evaluate :: forall m. MonadAff m => EvalRequest -> m (Maybe Calculation)
evaluate req = do
  let reqBody = Just $ AJAXB.json $ JSON.fromObject $ Object.empty
        # Object.insert "user" (JSON.fromString req.user)
        # Object.insert "expr" (exprToJSON req.expr)
  mResponse <- liftAff $
    AJAX.post json (fromSegment "/app/api/evaluate") reqBody
  pure do
    response <- hush mResponse
    hush $ CA.decode calculationCodec response.body

calculations :: forall m. MonadAff m => m (Maybe (Array Calculation))
calculations = do
  mResponse <- liftAff $
    AJAX.get json (fromSegment "/app/api/calculations")
  pure do
    response <- hush mResponse
    hush $ CA.decode (CA.array calculationCodec) response.body
