module Test.Main where


import Prelude

import Effect (Effect)
import Language.TestParser (testParser)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)


main :: Effect Unit
main = run [consoleReporter] do
  testParser
