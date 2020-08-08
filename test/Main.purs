module Test.Main where


import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Readme (readmeSpec)
import Test.GraphQL.Execution (executionSpec)
import Test.GraphQL.Execution.Result (executionResultSpec)
import Test.GraphQL.Language.Parser (parserSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  parserSpec
  executionSpec
  executionResultSpec
  readmeSpec
