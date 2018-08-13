module Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.GraphQL (graphql) as GraphQL
import Data.GraphQL.Type (ObjectType, Schema, argument, field, field', intScalar, objectType, schema, stringScalar) as GraphQL
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, runAff)
import Effect.Console (log)
import Effect.Exception (message)

main :: Effect (Fiber Unit)
main = runAff logResult $ GraphQL.graphql mySchema query unit
    where
      query = "{ hello square(value: 3) }"
      logResult (Left error) = log $ "Error: " <> message error
      logResult (Right result) = log $ stringify result

mySchema :: GraphQL.Schema Unit
mySchema = GraphQL.schema queryType Nothing

queryType :: GraphQL.ObjectType Unit
queryType = GraphQL.objectType "Query" (Just "The main query type")
    { hello
    , square
    }
  where
    hello = GraphQL.field' GraphQL.stringScalar Nothing \_ -> do
      delay $ Milliseconds 1000.0
      pure "Hello World"

    square = GraphQL.field
      GraphQL.intScalar
      (Just "Calculate the square of an int")
      { value: GraphQL.argument GraphQL.intScalar Nothing }
      resolve
        where
          resolve :: Unit -> { value :: Int } -> Aff Int
          resolve _ args = pure (args.value * args.value)
