module GraphQL
  ( graphql
  , module Execution
  , module Language
  , module GQL
  , module DSL
  , module Scalar
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.List (mapMaybe)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import GraphQL.Builtin.Scalar (boolean, float, id, int, string) as Scalar
import GraphQL.DSL (class ArgsDefToArgsParam, class ArgsFromRows, class Describe, class UnionDefinition, class UnionIntrospection, class UnionResolver, arg, argsFromDefinition, argsFromRows, describe, enumType, field, inputField, optionalInputField, inputObjectType, listField, nullableField, nullableListField, objectType, optionalArg, union, unionIntrospection, unionResolver, withArgument, withDefaultValue, withField, withInputField, withMappingResolver, withResolver, withSimpleResolver, (!!>), (!#>), (!>), (.>), (:>), (:?>), (?>)) as DSL
import GraphQL.Execution (execute) as Execution
import GraphQL.Language (parse) as Language
import GraphQL.Language.AST as AST
import GraphQL.OptionallyParallel (class OptionallyParallel)
import GraphQL.Type (class GraphQLType, class InputType, class OutputType, Argument, EnumType, ExecutionContext, Field, InputField(..), InputObjectType(..), ObjectType, ScalarType(..), Schema(..), UnionType, input, introspect, output) as GQL

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
  ∀ m a f.
  MonadError Error m =>
  OptionallyParallel f m =>
  GQL.Schema m a ->
  String ->
  Map String Json ->
  Maybe String ->
  a ->
  m Json
graphql schema query variables operation root =
  case Language.parse query of
  Left message -> throwError $ error $ "Parsing Error: " <> message
  Right document@(AST.DocumentNode { definitions }) ->
    let
      fragments = fromFoldable $ definitions # mapMaybe \def -> case def of
        f@(AST.FragmentDefinitionNode { name: AST.NameNode n }) -> pure $ Tuple n.value f
        _ -> Nothing
    in
      Execution.execute document schema { variables, fragments } operation (pure root)
