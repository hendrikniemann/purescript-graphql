module GraphQL where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Map (empty)
import Effect.Exception (Error, error)
import GraphQL.Execution (execute)
import GraphQL.Language (parse)
import GraphQL.Type (Schema)

graphql :: ∀ m a. MonadError Error m => Schema m a -> String -> a -> m Json
graphql schema query root = case parse query of
  Left message -> throwError $ error message
  Right document -> execute document schema empty root