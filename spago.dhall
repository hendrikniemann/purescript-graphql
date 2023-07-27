{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-graphql"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "functors"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "now"
  , "numbers"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "psci-support"
  , "record"
  , "spec"
  , "string-parsers"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
