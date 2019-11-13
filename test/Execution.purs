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
import GraphQL.Type ((!>), (.>), (:>), (?>), (!#>), (!!>))
import GraphQL.Type as GQL
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


testSchema :: GQL.Schema (Either Error) String
testSchema = GQL.Schema { query: queryType }


queryType :: GQL.ObjectType (Either Error) String
queryType =
  GQL.objectType "Query"
    .> "The root query type"
    :> GQL.field "hello" Scalar.string
      .> "A simple test field that connects the root string with a greeting."
      !> (\_ -> map (\p -> "Hello " <> p <> "!"))
    :> GQL.field "greet" Scalar.string
      .> "A field that takes a name and responds with a presonalized greeting."
      ?> GQL.arg Scalar.string (SProxy :: SProxy "name")
      !> (\{ name } _ -> pure $ "Greetings " <> name <> "!")
    :> GQL.field "test" Scalar.int
      !!> (_ $> 42)
    :> GQL.field "nested" userType
      !!> (_ $> User { id: "user1", name: "Hendrik", age: 25, level: NormalUser })
    :> GQL.listField "someList" Scalar.string
      !!> (_ $> ["This", "is", "a", "little", "list"])
    :> GQL.nullableField "nullable" Scalar.float
      !!> (_ $> Nothing)
    :> GQL.field "reflect" Scalar.string
      ?> GQL.arg userLevelType (SProxy :: _ "argIn")
      !> (\{ argIn } _ -> pure $ show argIn)
    :> GQL.field "toggle" Scalar.boolean
      ?> GQL.arg Scalar.boolean (SProxy :: _ "on")
      !> (\{ on: onArg } _ -> pure $ not onArg)


userType :: GQL.ObjectType (Either Error) User
userType =
  GQL.objectType "User"
    .> "A type for all users in the database"
    :> GQL.field "id" Scalar.string
      !#> unwrap >>> _.id
    :> GQL.field "name" Scalar.string
      !#> unwrap >>> _.name
    :> GQL.field "age" Scalar.int
      !#> unwrap >>> _.age
    :> GQL.field "level" userLevelType
      !#> unwrap >>> _.level

userLevelType :: GQL.EnumType UserLevel
userLevelType =
  GQL.enumType "UserLevel"
    .> "An enum type to denominate the user level."


testQuery :: String -> String -> Aff Unit
testQuery query expected = case map stringify (graphql testSchema query (pure "Hendrik")) of
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

    it "reads and serialized booleans correctly" do
      testQuery
        """{ toggle(on: true) }"""
        """{"data":{"toggle":false}}"""
      testQuery
        """{ toggle(on: false) }"""
        """{"data":{"toggle":true}}"""
