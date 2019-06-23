module Test.GraphQL.Execution where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.Map (empty, insert)
import GraphQL (graphql)
import GraphQL.Type (ObjectType, Schema(..), field, objectType)
import GraphQL.Type.Scalar as Scalar
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

testSchema :: Schema String
testSchema = Schema { query: queryType }

queryType :: ObjectType String
queryType = objectType
  (insert "hello" (field Scalar.string (\p -> "Hello " <> p <> "!"))
    (insert "test" (field Scalar.int (const 42)) empty))

executionSpec :: Spec Unit
executionSpec =
  describe "selection parser" do
    it "runs a simple field selection" do
      let result = graphql testSchema "query Test { hello }" "Hendrik"
      let strResult = map stringify result
      strResult `shouldEqual` Right "{\"hello\":\"Hello Hendrik!\"}"
    it "runs an aliased field selection" do
      let result = graphql testSchema "query Test { alias: hello }" "Hendrik"
      let strResult = map stringify result
      strResult `shouldEqual` Right "{\"alias\":\"Hello Hendrik!\"}"
    it "runs queries with multiple selections" do
      let result = graphql testSchema "query Test { hello test }" "Hendrik"
      let strResult = map stringify result
      strResult `shouldEqual` Right "{\"test\":42,\"hello\":\"Hello Hendrik!\"}"