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

import Calculator.Expr ( Expr, exprToJSON )
import Calculator.Calculation ( Calculation, calculationCodec )

type EvalRequest = { user :: String, expr :: Expr }

evaluate :: forall m. MonadAff m => EvalRequest -> m (Maybe Calculation)
evaluate req = do
  let reqBody = Just $ AJAXB.json $ JSON.fromObject $ Object.empty
        # Object.insert "user" (JSON.fromString req.user)
        # Object.insert "expr" (exprToJSON req.expr)
  mResponse <- liftAff $
    AJAX.post json "/app/api/evaluate" reqBody
  pure do
    response <- hush mResponse
    hush $ CA.decode calculationCodec response.body

calculations :: forall m. MonadAff m => m (Maybe (Array Calculation))
calculations = do
  mResponse <- liftAff $
    AJAX.get json "/app/api/calculations"
  pure do
    response <- hush mResponse
    hush $ CA.decode (CA.array calculationCodec) response.body
