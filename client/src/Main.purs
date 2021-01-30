module Main where

import Prelude

import Data.Maybe ( Maybe(..) )

import Effect ( Effect )
import Effect.Aff ( Aff, launchAff_ )
import Effect.Class ( liftEffect )

import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.VDom.Driver ( runUI )

import Routing.Duplex ( parse )
import Routing.Hash ( matchesWith )

import AppM ( runAppM )

import Calculator.Routes ( routeCodec )
import Calculator.Components.Router as Router

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    root :: H.Component HH.HTML Router.Query Unit Void Aff
    root = H.hoist runAppM Router.component

  halogenIO <- runUI root unit body

  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new

-- TODO: figure out some way to bring in config values for dev/prod
--       so that we can switch out the websocket links/base URL
