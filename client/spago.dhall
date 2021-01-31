{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "halogen"
  , "parsing"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
