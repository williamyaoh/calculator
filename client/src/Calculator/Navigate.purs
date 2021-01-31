module Calculator.Navigate
  ( class Navigate, navigate )
where

import Prelude

import Calculator.Routes ( Route )
import Halogen as H

class Monad m <= Navigate m where
  navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate ( H.HalogenM state action slots msg m ) where
  navigate = H.lift <<< navigate
