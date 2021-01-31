{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Calculator.Endpoints.Calculations
  ( API, handler )
where

import Data.Function ( (&) )

import Control.Monad.IO.Class     ( liftIO )
import Control.Monad.Reader.Class ( asks )

import Servant

import Opaleye

import Calculator.AppM
import Calculator.Database

type API = Get '[JSON] [Calculation]

handler :: AppM [Calculation]
handler = do
  conn <- asks appDB
  liftIO $ runSelect @_ @Calculation conn last10Calculations

last10Calculations :: Select CalculationRead
last10Calculations =
  selectTable calculation
    & orderBy (desc calculationCreatedAt)
    & limit 10
