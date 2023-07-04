module Test.GraphQL.Execution where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Parallel (class Parallel)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Core as Json
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..), either)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (trim)
import Effect.Aff (Aff, Error, throwError, error)
import GraphQL ((!!>), (!#>), (!>), (.>), (:>), (?>))
import GraphQL as GQL
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

newtype ParrEither a b = ParrEither (Either a b)

derive instance Newtype (ParrEither a b) _

derive newtype instance Functor (ParrEither a)

derive newtype instance Apply (ParrEither a)

derive newtype instance Applicative (ParrEither a)

derive newtype instance Bind (ParrEither a)

derive newtype instance Monad (ParrEither a)

derive newtype instance MonadError a (ParrEither a)

derive newtype instance MonadThrow a (ParrEither a)

instance Parallel (ParrEither a) (ParrEither a) where
  parallel = identity
  sequential = identity

-- Some data types for out upcoming tests

newtype User = User { id :: String, name :: String, age :: Int, level :: UserLevel }

derive instance newtypeUser :: Newtype User _

data UserLevel = NormalUser | Moderator | Admin

derive instance genericUserLevel :: Generic UserLevel _

derive instance Eq UserLevel

derive instance Ord UserLevel

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


testSchema :: GQL.Schema (ParrEither Error) String
testSchema = GQL.Schema { query: queryType, mutation: Nothing }


queryType :: GQL.ObjectType (ParrEither Error) String
queryType =
  GQL.objectType "Query"
    .> "The root query type"
    :> GQL.field "hello" GQL.string
      .> "A simple test field that connects the root string with a greeting."
      !> (\_ p -> pure $ "Hello " <> p <> "!")
    :> GQL.field "greet" GQL.string
      .> "A field that takes a name and responds with a presonalized greeting."
      ?> GQL.arg GQL.string (Proxy :: Proxy "name")
      !> (\{ name } _ -> pure $ "Greetings " <> name <> "!")
    :> GQL.field "test" GQL.int
      !#> const 42
    :> GQL.field "nested" userType
      !#> (const $ User { id: "user1", name: "Hendrik", age: 25, level: NormalUser })
    :> GQL.listField "someList" GQL.string
      !#> (const ["This", "is", "a", "little", "list"])
    :> GQL.nullableField "nullable" GQL.float
      !#> (const Nothing)
    :> GQL.field "reflect" GQL.string
      ?> GQL.arg userLevelType (Proxy :: _ "argIn")
      !> (\{ argIn } _ -> pure $ show argIn)
    :> GQL.field "reflectStringOptional" GQL.string
      ?> GQL.optionalArg GQL.string (Proxy :: _ "argIn")
      !> (\{ argIn } _ -> pure $ fromMaybe "default" argIn)
    :> GQL.field "toggle" GQL.boolean
      ?> GQL.arg GQL.boolean (Proxy :: _ "on")
      !> (\{ on: onArg } _ -> pure $ not onArg)
    :> GQL.nullableField "fail" GQL.boolean
      !!> (\_ -> throwError $ error "Always fails at runtime.")


userType :: GQL.ObjectType (ParrEither Error) User
userType =
  GQL.objectType "User"
    .> "A type for all users in the database"
    :> GQL.field "id" GQL.string
      !#> unwrap >>> _.id
    :> GQL.field "name" GQL.string
      !#> unwrap >>> _.name
    :> GQL.field "age" GQL.int
      !#> unwrap >>> _.age
    :> GQL.field "level" userLevelType
      !#> unwrap >>> _.level


userLevelType :: GQL.EnumType UserLevel
userLevelType =
  GQL.enumType "UserLevel"
    .> "An enum type to denominate the user level."


testQuery :: String -> String -> Aff Unit
testQuery query expected =
  case GQL.graphql testSchema query Map.empty Nothing "Hendrik" # unwrap of
  Right res -> stringify res `shouldEqual` expected

  Left message -> fail $ show message

affFromEither :: ParrEither Error ~> Aff
affFromEither = unwrap >>> either throwError pure

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

    it "catches errors thrown inside of resolvers" $
      testQuery
        """{ fail }""" $
        trim """
          {"data":{"fail":null},"errors":[{"message":"Always fails at runtime.","path":["fail"]}]}
        """

    it "returns the __typename meta field" do
      testQuery
        """{ __typename }"""
        """{"data":{"__typename":"Query"}}"""
      testQuery
        """{ nested { __typename } }"""
        """{"data":{"nested":{"__typename":"User"}}}"""

    it "wraps optional parameters in maybe" do
      testQuery
        """{ reflectStringOptional }"""
        """{"data":{"reflectStringOptional":"default"}}"""
      testQuery
        """{ reflectStringOptional(argIn: "Hello World") }"""
        """{"data":{"reflectStringOptional":"Hello World"}}"""

    it "accepts null as a value for an optional parameter" $
      testQuery
        """{ reflectStringOptional(argIn: null) }"""
        """{"data":{"reflectStringOptional":"default"}}"""

    it "accepts null as a variable value for an optional parameter" $ affFromEither do
      let query = "query ($arg: String) { reflectStringOptional(argIn: $arg) }"
      res <- GQL.graphql testSchema query (Map.insert "arg" Json.jsonNull Map.empty) Nothing ""
      stringify res `shouldEqual` """{"data":{"reflectStringOptional":"default"}}"""


introspectionSpec :: Spec Unit
introspectionSpec =
  describe "Introspection" do
    describe "__type" do
      it "returns type introspection by their name" do
        testQuery
          """{ __type(name: "User") { name } }"""
          """{"data":{"__type":{"name":"User"}}}"""

      it "returns null if no type with given name exists" do
        testQuery
          """{ __type(name: "NotFound") { name } }"""
          """{"data":{"__type":null}}"""

    describe "__Type" do
      it "correctly returns data for object types" do
        testQuery
          """{ __type(name: "User") { name kind description } }"""
          """{"data":{"__type":{"name":"User","kind":"OBJECT","description":"A type for all users in the database"}}}"""
