module Test.GraphQL.Execution.Result where

import Prelude

import Data.Argonaut.Core (fromString, stringify)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import GraphQL.Execution.Result (Result(..), serializeResult)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

executionResultSpec :: Spec Unit
executionResultSpec =
  describe "serializeResult" do
    it "serializes a simple result" do
      let r = ResultObject (Tuple "helloWorld" (ResultLeaf $ fromString "Hello World!"):Nil)
      stringify (serializeResult r) `shouldEqual` """{"data":{"helloWorld":"Hello World!"}}"""
    it "serializes lists of values" do
      let r = ResultObject ((Tuple "list"
                (ResultList $ (ResultLeaf $ fromString "a"):(ResultLeaf $ fromString "b"):Nil)
              ):Nil)
      stringify (serializeResult r) `shouldEqual` """{"data":{"list":["a","b"]}}"""
    it "serializes a more complex result" do
      let r = ResultObject (
                (Tuple "a" (ResultLeaf $ fromString "A")):
                (Tuple "b" (ResultLeaf $ fromString "B")):
                Nil
              )
      stringify (serializeResult r) `shouldEqual` """{"data":{"a":"A","b":"B"}}"""