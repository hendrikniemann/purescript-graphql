module Test.Main where


import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Language.TestParser (testParser)
import Test.GraphQL.Execution (executionSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  testParser
  executionSpec
