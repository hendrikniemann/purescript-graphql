{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-graphql"
, dependencies =
  [ "argonaut"
  , "console"
  , "control"
  , "effect"
  , "enums"
  , "foldable-traversable"
  , "nullable"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "record"
  , "spec"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
