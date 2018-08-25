module Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.GraphQL (graphql) as GraphQL
import Data.GraphQL.Type (ObjectType, Schema, argument, field, field', id, int, nonNull, objectType, schema, string) as GraphQL
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, runAff)
import Effect.Console (log)
import Effect.Exception (message)

main :: Effect (Fiber Unit)
main = runAff logResult $ GraphQL.graphql mySchema query unit
    where
      query = "{ hello square(value: 3) user(id: \"12\") { id lastName } }"
      logResult (Left error) = log $ "Error: " <> message error
      logResult (Right result) = log $ stringify result

mySchema :: GraphQL.Schema Unit
mySchema = GraphQL.schema queryType Nothing

queryType :: GraphQL.ObjectType Unit
queryType = GraphQL.objectType "Query" (Just "The main query type")
    { hello
    , square
    , user
    }
  where
    hello = GraphQL.field' (GraphQL.nonNull GraphQL.string) Nothing \_ -> do
      delay $ Milliseconds 1000.0
      pure "Hello World"

    square = GraphQL.field
      (GraphQL.nonNull GraphQL.int)
      (Just "Calculate the square of an int")
      { value: GraphQL.argument GraphQL.int Nothing }
      resolve
        where
          resolve :: Unit -> { value :: Maybe Int } -> Aff Int
          resolve _ { value: (Just x)} = pure (x * x)
          resolve _ _                  = pure 0
    
    user = GraphQL.field userType Nothing args resolve
      where
        args = { id: GraphQL.argument (GraphQL.nonNull GraphQL.id) Nothing }
        resolve :: Unit -> { id :: String } -> Aff User
        resolve _ { id } = pure { id, firstName: "Dummy", lastName: "User" }

type User =
  { id :: String
  , firstName :: String
  , lastName :: String
  }

userType :: GraphQL.ObjectType User
userType = GraphQL.objectType "User" (Just "A type for a user.")
  { id: GraphQL.field' (GraphQL.nonNull GraphQL.id) Nothing (_.id >>> pure)
  , firstName: GraphQL.field' (GraphQL.nonNull GraphQL.string) Nothing (_.firstName >>> pure)
  , lastName: GraphQL.field' (GraphQL.nonNull GraphQL.string) Nothing (_.lastName >>> pure)
  }
 