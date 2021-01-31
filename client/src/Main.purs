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

import Web.HTML ( window )
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as Document

import Routing.Duplex ( parse )
import Routing.Hash ( matchesWith )

import Env ( env )
import AppM ( runAppM )

import Calculator.Routes ( routeCodec )
import Calculator.Components.Router as Router

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  document <- liftEffect $ Window.document =<< window

  liftEffect $ Document.setTitle env.title document

  let
    root :: H.Component HH.HTML Router.Query Unit Void Aff
    root = H.hoist runAppM Router.component

  halogenIO <- runUI root unit body

  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new

-- TODO: add some css
-- TODO: set up a Shakefile for all this
-- TODO: set up a Heroku buildpack for all this
-- TODO: handle division by zero, probably by signalling an error
-- TODO: once we're done, go back through and remove any unnecessary logs and imports
