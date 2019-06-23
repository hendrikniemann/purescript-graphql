module GraphQL where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Either (Either)
import GraphQL.Execution (execute)
import GraphQL.Language (parse)
import GraphQL.Type (Schema)

graphql :: ∀ a. Schema a -> String -> a -> Either String Json
graphql schema query root = do
  document <- parse query
  pure $ execute document schema root