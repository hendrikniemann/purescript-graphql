module GraphQL where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect.Exception (Error, error)
import GraphQL.Execution (execute)
import GraphQL.Language (parse)
import GraphQL.Type (Schema, VariableMap)

-- | Parses a GraphQL query string into a document and then executes the query given the parameters
-- |
-- | Parameters:
-- | - *schema*: the schema the query is evaluated against
-- | - *query*: the GraphQL query string
-- | - *variables*: a map of variables with their corresponding JSON values
-- | - *operation*: an optional operation name - can be omitted if query contains only one operation
-- | - *root*: a root value to be passed to the query and mutation resolvers
graphql ::
  ∀ m a.
  MonadError Error m =>
  Schema m a ->
  String ->
  VariableMap ->
  Maybe String ->
  m a ->
  m Json
graphql schema query variables operation root = case parse query of
  Left message -> throwError $ error $ "Parsing Error: " <> message
  Right document -> execute document schema variables operation root
