module Calculator.Routes
  ( Route(..), routeCodec )
where

import Prelude hiding ( (/) )

import Data.Generic.Rep ( class Generic )
import Data.Generic.Rep.Show ( genericShow )

import Routing.Duplex ( RouteDuplex', root )
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ( (/) )

data Route
  = SelfIntro
  | Calculator

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ G.sum
  { "SelfIntro": "selfintro" / G.noArgs
  , "Calculator": "calculator" / G.noArgs
  }
