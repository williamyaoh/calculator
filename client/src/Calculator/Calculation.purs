module Calculator.Calculation
  ( Calculation, calculationCodec )
where

import Prelude

import Prim.Row ( class Lacks, class Cons )

import Data.Symbol ( class IsSymbol, SProxy(..) )
import Data.Codec as C
import Data.Codec.Argonaut as CA

import Record as R

type Calculation =
  { id :: Int
  , text :: String
  , result :: Number
  , user :: String
  }

recordPropRename :: forall prev next a r0 r1 r2.
                    IsSymbol prev
                 => IsSymbol next
                 => Lacks prev r0
                 => Lacks next r0
                 => Cons prev a r0 r1
                 => Cons next a r0 r2
                 => SProxy prev
                 -> SProxy next
                 -> CA.JsonCodec a
                 -> CA.JPropCodec (Record r0)
                 -> CA.JPropCodec (Record r2)
recordPropRename prev next prop codec =
  C.mapCodec (pure <<< R.rename prev next) (R.rename next prev) $
    CA.recordProp prev prop codec

calculationCodec :: CA.JsonCodec Calculation
calculationCodec =
  CA.object "Calculation" $ CA.record
    # CA.recordProp (SProxy :: _ "id") CA.int
    # recordPropRename (SProxy :: _ "calculation_text") (SProxy :: _ "text") CA.string
    # recordPropRename (SProxy :: _ "calculation_result") (SProxy :: _ "result") CA.number
    # CA.recordProp (SProxy :: _ "user") CA.string
