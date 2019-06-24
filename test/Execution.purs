module Test.GraphQL.Execution where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import GraphQL (graphql)
import GraphQL.Type (ObjectType, Schema(..), field, objectType, (:>), (.>), (!>))
import GraphQL.Type.Scalar as Scalar
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

testSchema :: Schema String
testSchema = Schema { query: queryType }

queryType :: ObjectType String
queryType =
  objectType "Query"
    .> "The root query type"
    :> field "hello" Scalar.string
      .> "A simple test field that connects the root string with a greeting."
      !> (\p -> "Hello " <> p <> "!")
    :> field "test" Scalar.int
      !> const 42
    :> field "nested" userType
      !> const (User { id: "user1", name: "Hendrik", age: 25 })

newtype User = User { id :: String, name :: String, age :: Int }

userType :: ObjectType User
userType =
  objectType "User"
    .> "A type for all users in the database"
    :> field "id" Scalar.string
      !> (\(User user) -> user.id)
    :> field "name" Scalar.string
      !> (\(User user) -> user.name)
    :> field "age" Scalar.int
      !> (\(User user) -> user.age)

executionSpec :: Spec Unit
executionSpec =
  describe "selection parser" do
    it "runs a simple field selection" do
      let result = graphql testSchema "query Test { hello }" "Hendrik"
      let strResult = map stringify result
      strResult `shouldEqual` Right """{"hello":"Hello Hendrik!"}"""
    it "runs an aliased field selection" do
      let result = graphql testSchema "query Test { alias: hello }" "Hendrik"
      let strResult = map stringify result
      strResult `shouldEqual` Right """{"alias":"Hello Hendrik!"}"""
    it "runs queries with multiple selections" do
      let result = graphql testSchema "query Test { hello test }" "Hendrik"
      let strResult = map stringify result
      strResult `shouldEqual` Right """{"hello":"Hello Hendrik!","test":42}"""
    it "runs nested queries" do
      let result = graphql testSchema "query Test { nested { id name age } }" ""
      let strResult = map stringify result
      strResult `shouldEqual` Right """{"nested":{"id":"user1","name":"Hendrik","age":25}}"""
