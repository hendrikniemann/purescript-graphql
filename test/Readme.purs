module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (Error)
import GraphQL (graphql)
import GraphQL.Type ((!>), (.>), (:>))
import GraphQL.Type as GraphQL
import GraphQL.Type.Scalar as Scalar
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main = case graphql schema "{ hello }" (pure unit) of
  Left error -> Console.error $ show error
  Right result -> Console.log $ stringify result
-- {"data":{"hello":"world"}}

schema :: GraphQL.Schema (Either Error) Unit
schema = GraphQL.Schema { query: queryType }

queryType :: GraphQL.ObjectType (Either Error) Unit
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
    it "should execute the given query" $
        case graphql schema "{ hello }" (pure unit) of
          Left error -> throwError error
          Right result -> stringify result `shouldEqual` """{"data":{"hello":"world"}}"""