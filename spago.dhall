{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "console"
  , "const"
  , "css"
  , "datetime"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "maybe"
  , "newtype"
  , "now"
  , "prelude"
  , "psci-support"
  , "refs"
  , "routing"
  , "routing-duplex"
  , "transformers"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
