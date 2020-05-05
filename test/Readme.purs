module Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import GraphQL (graphql)
import GraphQL.Type ((!>), (.>), (:>))
import GraphQL.Type as GraphQL
import GraphQL.Type.Scalar as Scalar
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main = do
  result <- graphql schema "{ hello }" Map.empty Nothing (pure unit)
  Console.log $ stringify result
-- {"data":{"hello":"world"}}

schema :: GraphQL.Schema Effect Unit
schema = GraphQL.Schema { query: queryType, mutation: Nothing }

queryType :: GraphQL.ObjectType Effect Unit
queryType =
  GraphQL.objectType "Query"
    .> "The root query type."
    :> GraphQL.field "hello" Scalar.string
      .> "A simple field that always returns \"world\"."
      !> (\_ _ -> pure "world")

-- | A spec that tests that the example in the README actually works
readmeSpec :: Spec Unit
readmeSpec =
  describe "Readme" $
    it "should execute the given query" $ liftEffect do
      result <- graphql schema "{ hello }" Map.empty Nothing (pure unit)
      stringify result `shouldEqual` """{"data":{"hello":"world"}}"""