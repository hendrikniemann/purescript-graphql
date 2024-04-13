module Test.Async where

import Prelude

import Data.Argonaut (stringify)
import Data.DateTime as DateTime
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import GraphQL ((.>), (:>), (!>))
import GraphQL as GraphQL
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

schema :: GraphQL.Schema Aff Unit
schema = GraphQL.Schema { query: queryType, mutation: Nothing }

queryType :: GraphQL.ObjectType Aff Unit
queryType =
  GraphQL.objectType "Query"
    :> "The root query type."
    .> GraphQL.field "async" GraphQL.string
      :> "An asynchronous field."
      !> asyncResolver

asyncResolver :: forall parent args. parent -> args -> Aff String
asyncResolver _ _ = do
  delay (Milliseconds 1000.0)
  pure "Hello, World!"

asyncTest :: Spec Unit
asyncTest = describe "Parallel" $ do
  it "resolves two fields in parallel" $ do
    let query = "{ a: async b: async }"
    startedAt <- liftEffect nowDateTime
    result <- GraphQL.graphql schema query Map.empty Nothing unit
    finishedAt <- liftEffect nowDateTime
    stringify result `shouldEqual` "{\"data\":{\"a\":\"Hello, World!\",\"b\":\"Hello, World!\"}}"
    DateTime.diff finishedAt startedAt `shouldSatisfy` (_ < Milliseconds 2000.0)
