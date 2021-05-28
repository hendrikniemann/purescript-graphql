{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-graphql"
, dependencies =
  [ "argonaut"
  , "arrays"
  , "bifunctors"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "numbers"
  , "ordered-collections"
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
