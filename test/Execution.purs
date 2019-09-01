module Test.GraphQL.Execution where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, Error)
import GraphQL (graphql)
import GraphQL.Type (ObjectType, Schema(..), arg, field, objectType, (!>), (.>), (:>), (?>))
import GraphQL.Type.Scalar as Scalar
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

testSchema :: Schema (Either Error) String
testSchema = Schema { query: queryType }

queryType :: ObjectType (Either Error) String
queryType =
  objectType "Query"
    .> "The root query type"
    :> field "hello" Scalar.string
      .> "A simple test field that connects the root string with a greeting."
      !> (\_ p -> pure $ "Hello " <> p <> "!")
    :> field "greet" Scalar.string
      .> "A field that takes a name and responds with a presonalized greeting."
      ?> arg Scalar.string (SProxy :: SProxy "name")
      !> (\{ name } _ -> pure $ "Greetings " <> name <> "!")
    :> field "test" Scalar.int
      !> (\_ _ -> pure 42)
    :> field "nested" userType
      !> (\_ _ -> pure $ User { id: "user1", name: "Hendrik", age: 25 })

newtype User = User { id :: String, name :: String, age :: Int }

userType :: ObjectType (Either Error) User
userType =
  objectType "User"
    .> "A type for all users in the database"
    :> field "id" Scalar.string
      !> (\_ (User user) -> pure user.id)
    :> field "name" Scalar.string
      !> (\_ (User user) -> pure user.name)
    :> field "age" Scalar.int
      !> (\_ (User user) -> pure user.age)

testQuery :: String -> String -> Aff Unit
testQuery query expected = case map stringify (graphql testSchema query "Hendrik") of
    Right res -> res `shouldEqual` expected
    Left _ -> fail "failed to execute query"

executionSpec :: Spec Unit
executionSpec =
  describe "Executor" do
    it "runs a simple field selection" $
      testQuery
        "query Test { hello }"
        """{"data":{"hello":"Hello Hendrik!"}}"""
    it "runs an aliased field selection" do
      testQuery
        "query Test { alias: hello }"
        """{"data":{"alias":"Hello Hendrik!"}}"""
    it "runs queries with multiple selections" do
      testQuery
        "query Test { hello test }"
        """{"data":{"hello":"Hello Hendrik!","test":42}}"""
    it "runs nested queries" do
      testQuery
        "query Test { nested { id name age } }"
        """{"data":{"nested":{"id":"user1","name":"Hendrik","age":25}}}"""
    it "runs a query with a field that has an argument" do
      testQuery
        """query Test { greet(name: "Hendrik") }"""
        """{"data":{"greet":"Greetings Hendrik!"}}"""
