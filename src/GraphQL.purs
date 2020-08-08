module GraphQL where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.List (mapMaybe)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import GraphQL.Execution (execute)
import GraphQL.Language (parse)
import GraphQL.Language.AST as AST
import GraphQL.Type (Schema)

-- | Parses a GraphQL query string into a document and then executes the query given the parameters
-- |
-- | Parameters:
-- | - *schema*: the schema the query is evaluated against
-- | - *query*: the GraphQL query string
-- | - *variables*: a map of variables with their corresponding JSON values
-- | - *operation*: an optional operation name - can be omitted if query contains only one operation
-- | - *root*: a root value to be passed to the query and mutation resolvers
-- |
-- | @see https://spec.graphql.org/June2018/#sec-Execution
graphql ::
  ∀ m a.
  MonadError Error m =>
  Schema m a ->
  String ->
  Map String Json ->
  Maybe String ->
  a ->
  m Json
graphql schema query variables operation root =
  case parse query of
  Left message -> throwError $ error $ "Parsing Error: " <> message
  Right document@(AST.DocumentNode { definitions }) ->
    let
      fragments = fromFoldable $ definitions # mapMaybe \def -> case def of
        f@(AST.FragmentDefinitionNode { name: AST.NameNode n }) -> pure $ Tuple n.value f
        _ -> Nothing
    in
      execute document schema { variables, fragments } operation (pure root)
