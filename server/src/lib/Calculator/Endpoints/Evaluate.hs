{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Calculator.Endpoints.Evaluate where

import Data.Aeson    as Aeson
import Data.Foldable ( traverse_ )
import Data.Text     ( Text )

import Control.Concurrent.MVar    ( withMVar )
import Control.Monad.IO.Class     ( liftIO )
import Control.Monad.Reader.Class ( asks )

import qualified Network.WebSockets as WS
import           Servant

import Opaleye

import Calculator.AppM
import Calculator.Database
import Calculator.Expr     as Expr

type API = ReqBody '[JSON] Request :> Post '[JSON] Calculation

-- By the time the Expr gets here, we've successfully validated that it
-- has a valid structure. So all we have to do is evaluate it.

data Request = Request
  { reqUser :: Text
  , reqExpr :: Expr
  }

instance FromJSON Request where
  parseJSON = withObject "Evaluate.Request" $ \obj -> Request
    <$> obj .: "user"
    <*> obj .: "expr"

handler :: Request -> AppM Calculation
handler req = do
  let result = Expr.evaluate $ reqExpr req
  if isInfinite result || isNaN result
    then throwError err400
    else do
      let text = Expr.toText $ reqExpr req
          toWrite = Calculation
            () (sqlStrictText text) (sqlDouble result) (sqlStrictText $ reqUser req) ()
      calc <- insertCalc toWrite
      propagateCalc calc
      pure $! calc

propagateCalc :: Calculation -> AppM ()
propagateCalc calc = do
  clients <- asks appClients
  liftIO $ withMVar clients $
    traverse_ $ \(_, conn) ->
      WS.sendTextData conn $ Aeson.encode calc

insertCalc :: CalculationWrite -> AppM Calculation
insertCalc cw = do
  conn <- asks appDB
  calcs <- liftIO $ runInsert_ conn $ Insert
    { iTable = calculation
    , iRows = [ cw ]
    , iReturning = rReturning id
    , iOnConflict = Nothing
    }
  case calcs of
    [calc] -> pure $! calc
    _other -> throwError $ err500
