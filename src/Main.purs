module Main where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Array (head)
import Data.Either (Either(..), either)
import Data.GraphQL as GraphQL
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber, Milliseconds(..), delay, runAff, throwError)
import Effect.Console (log)
import Effect.Exception (message)

main :: Effect (Fiber Unit)
main = runAff logResult $ runQuery mySchema "{ square(value: 3) }"
    where
      logResult :: Either Error Json -> Effect Unit
      logResult (Left error) = log $ "Error: " <> message error
      logResult (Right result) = log $ stringify result

runQuery :: GraphQL.Schema -> String -> Aff Json
runQuery schema query = do
  document <- either throwError pure $ GraphQL.parse query
  let errors = GraphQL.validate schema document
  maybe (pure unit) throwError $ head errors
  GraphQL.execute schema document

mySchema :: GraphQL.Schema
mySchema = GraphQL.schema queryType Nothing

queryType :: GraphQL.GraphQLType Unit
queryType = GraphQL.objectType "Query" (Just "The main query type")
    { hello
    , square
    }
  where
    hello = GraphQL.field' GraphQL.stringScalar Nothing \_ -> do
      delay $ Milliseconds 2000.0
      pure "Hello World"

    square :: GraphQL.Field Unit
    square = GraphQL.field
      GraphQL.intScalar
      (Just "Calculate the square of an int")
      { value: GraphQL.argument GraphQL.intScalar Nothing }
      resolve
        where
          resolve :: Unit -> { value :: Int } -> Aff Int
          resolve _ args = pure (args.value * args.value)
