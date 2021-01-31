{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Calculator.Database
  ( CalculationT(..), Calculation
  , CalculationWrite, CalculationRead
  , calculation
  )
where

import Data.Aeson
import Data.Int                   ( Int64 )
import Data.Profunctor.Product.TH ( makeAdaptorAndInstanceInferrable )
import Data.Text                  ( Text )
import Data.Time                  ( UTCTime )

import Opaleye

data CalculationT a b c d e = Calculation
  { calculationID        :: a
  , calculationText      :: b
  , calculationResult    :: c
  , calculationUser      :: d
  , calculationCreatedAt :: e
  }
type Calculation = CalculationT Int64 Text Double Text UTCTime

instance ToJSON Calculation where
  toJSON (Calculation id text result user createdAt) = object
    [ "id" .= id
    , "calculation_text" .= text
    , "calculation_result" .= result
    , "user" .= user
    , "created_at" .= createdAt
    ]

makeAdaptorAndInstanceInferrable "pCalculation" ''CalculationT

type F field = Field field

type CalculationWrite = CalculationT () (F SqlText) (F SqlFloat8) (F SqlText) ()
type CalculationRead = CalculationT (F SqlInt8) (F SqlText) (F SqlFloat8) (F SqlText) (F SqlTimestamptz)

calculation :: Table CalculationWrite CalculationRead
calculation = table "calculation" $
  pCalculation $ Calculation
    { calculationID = readOnlyTableField "id"
    , calculationText = requiredTableField "calculation_text"
    , calculationResult = requiredTableField "calculation_result"
    , calculationUser = requiredTableField "username"
    , calculationCreatedAt = readOnlyTableField "created_at"
    }
