module Test.GraphQL.Execution where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.Enum (class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericPred, genericSucc)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, Error)
import GraphQL (graphql)
import GraphQL.Type (EnumType, ObjectType, Schema(..), arg, enumType, field, listField, nullableField, objectType, (!>), (.>), (:>), (?>))
import GraphQL.Type.Scalar as Scalar
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- Some data types for out upcoming tests

newtype User = User { id :: String, name :: String, age :: Int, level :: UserLevel }

derive instance newtypeUser :: Newtype User _

data UserLevel = NormalUser | Moderator | Admin

derive instance genericUserLevel :: Generic UserLevel _

instance eqUserLevel :: Eq UserLevel where
  eq = genericEq

instance ordUserLevel :: Ord UserLevel where
  compare = genericCompare

instance boundedUserLevel :: Bounded UserLevel where
  top = genericTop
  bottom = genericBottom

instance enumUserLevel :: Enum UserLevel where
  succ = genericSucc
  pred = genericPred

instance showUserLevel :: Show UserLevel where
  show Admin = "ADMIN"
  show Moderator = "MODERATOR"
  show NormalUser = "USER"


testSchema :: Schema (Either Error) String
testSchema = Schema { query: queryType }


queryType :: ObjectType (Either Error) String
queryType =
  objectType "Query"
    .> "The root query type"
    :> field "hello" Scalar.string
      .> "A simple test field that connects the root string with a greeting."
      !> (\_ -> map (\p -> "Hello " <> p <> "!"))
    :> field "greet" Scalar.string
      .> "A field that takes a name and responds with a presonalized greeting."
      ?> arg Scalar.string (SProxy :: SProxy "name")
      !> (\{ name } _ -> pure $ "Greetings " <> name <> "!")
    :> field "test" Scalar.int
      !> (\_ _ -> pure 42)
    :> field "nested" userType
      !> (\_ _ -> pure $ User { id: "user1", name: "Hendrik", age: 25, level: NormalUser })
    :> listField "someList" Scalar.string
      !> (\_ _ -> pure $ ["This", "is", "a", "little", "list"])
    :> nullableField "nullable" Scalar.float
      !> (\_ _ -> pure Nothing)
    :> field "reflect" Scalar.string
      ?> arg userLevelType (SProxy :: _ "argIn")
      !> (\{ argIn } _ -> pure $ show argIn)


userType :: ObjectType (Either Error) User
userType =
  objectType "User"
    .> "A type for all users in the database"
    :> field "id" Scalar.string
      !> (\_ -> map $ unwrap >>> _.id)
    :> field "name" Scalar.string
      !> (\_ -> map $ unwrap >>> _.name)
    :> field "age" Scalar.int
      !> (\_ -> map $ unwrap >>> _.age)
    :> field "level" userLevelType
      !> (\_ -> map $ unwrap >>> _.level)


userLevelType :: EnumType UserLevel
userLevelType =
  enumType "UserLevel"
    .> "An enum type to denominate the user level."


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

    it "runs an aliased field selection" $
      testQuery
        "query Test { alias: hello }"
        """{"data":{"alias":"Hello Hendrik!"}}"""

    it "runs queries with multiple selections" $
      testQuery
        "query Test { hello test }"
        """{"data":{"hello":"Hello Hendrik!","test":42}}"""

    it "runs nested queries" $
      testQuery
        "query Test { nested { id name age } }"
        """{"data":{"nested":{"id":"user1","name":"Hendrik","age":25}}}"""

    it "runs a query with a field that has an argument" $
      testQuery
        """query Test { greet(name: "Hendrik") }"""
        """{"data":{"greet":"Greetings Hendrik!"}}"""

    it "runs a query that queries a field that is a list" $
      testQuery
        "query Test { someList }"
        """{"data":{"someList":["This","is","a","little","list"]}}"""

    it "runs a query that queries a field that is nullable" $
      testQuery
        "query Test { nullable }"
        """{"data":{"nullable":null}}"""

    it "serialises enums correctly" $
      testQuery
        """{ nested { level } }"""
        """{"data":{"nested":{"level":"USER"}}}"""

    it "reads in enums correctly" do
      testQuery
        """{ reflect(argIn: USER) }"""
        """{"data":{"reflect":"USER"}}"""
      testQuery
        """{ reflect(argIn: MODERATOR) }"""
        """{"data":{"reflect":"MODERATOR"}}"""
      testQuery
        """{ reflect(argIn: ADMIN) }"""
        """{"data":{"reflect":"ADMIN"}}"""