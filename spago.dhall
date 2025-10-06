{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "bifunctors"
  , "console"
  , "const"
  , "css"
  , "datetime"
  , "dom-indexed"
  , "effect"
  , "either"
  , "foreign-generic"
  , "halogen"
  , "halogen-css"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "refs"
  , "routing"
  , "routing-duplex"
  , "strings"
  , "transformers"
  , "tuples"
  , "uuid"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "../server/src/Data/Api/**/*.purs"
  , "../server/src/Entity/**/*.purs"
  , "test/**/*.purs"
  ]
}
