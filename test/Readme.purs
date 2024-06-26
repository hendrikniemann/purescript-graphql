module Test.Readme where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Argonaut.Core (stringify)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import GraphQL ((!>), (.>), (:>), (?>), graphql)
import GraphQL as GraphQL
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- main :: Effect Unit
-- main = do
--   result <- graphql schema """{ hello(name: "world") }""" Map.empty Nothing unit
--   Console.log $ stringify result -- {"data":{"hello":"world"}}

schema :: GraphQL.Schema (ReaderT String Effect) Unit
schema = GraphQL.Schema { query: queryType, mutation: Nothing }

queryType :: GraphQL.ObjectType (ReaderT String Effect) Unit
queryType =
  GraphQL.objectType "Query"
    :> "The root query type."
    .> GraphQL.field "hello" GraphQL.string
      ?> GraphQL.arg @"name" GraphQL.string
      :> "A simple field that returns a greeting."
      !> (\{ name } _ -> pure $ "Hello, " <> name <> "!")

-- | A spec that tests that the example in the README actually works
readmeSpec :: Spec Unit
readmeSpec =
  describe "Readme" $
    it "should execute the given query" $ liftEffect do
      let reader = graphql schema """{ hello(name: "Stranger") }""" Map.empty Nothing unit
      result <- runReaderT reader "Stranger"
      stringify result `shouldEqual` """{"data":{"hello":"Hello, Stranger!"}}"""
