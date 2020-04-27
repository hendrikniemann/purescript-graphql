module GraphQL.Execution (execute) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Encode ((:=), (~>))
import Data.List (List(..), find)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Exception (Error, error)
import GraphQL.Execution.Result (serializeResult)
import GraphQL.Language.AST (DefinitionNode(..), DocumentNode(..), NameNode(..), OperationTypeNode(..))
import GraphQL.Type (ObjectType, Schema(..), ExecutionContext, field, output, (!#>), (:>), introspect)
import GraphQL.Type.Introspection (schemaType)
import GraphQL.Type.Introspection.Datatypes (SchemaIntrospection(..))


simpleError :: forall m. Applicative m => String -> m Json
simpleError e = pure $ "error" := e ~> jsonEmptyObject


execute :: forall m a.
  MonadError Error m =>
  DocumentNode ->
  Schema m a ->
  ExecutionContext ->
  Maybe String ->
  m a ->
  m Json
execute (DocumentNode { definitions }) (Schema s) variables operation root = do
  -- Find the operation with the operation name given supplied. If no operation name is supplied,
  -- return the only operation of the document. If multiple operations are supplied without an
  -- operation name, or the named operation cannot be found in the document, throw an error.
  definitionNode <- case operation of
    Just operationName ->
      fromMaybe
        (throwError $ error $ "Could not find definitions with name " <> operationName <> ".")
        (pure <$> find (hasOperationName operationName) definitions)

    Nothing -> case definitions of
      Nil -> throwError $ error "No definitions found in document."
      Cons query Nil -> pure query
      _ -> throwError $ error "Operation name is required for queries with multiple operations."

  executionResult <- case definitionNode of
    OperationDefinitionNode { operation: Query, selectionSet } ->
      let
        schemaIntrospection =
          SchemaIntrospection
            { queryType: introspect s.query
            , mutationType: map introspect s.mutation
            }
        metaQuery = s.query
          :> field "__schema" (schemaType :: ObjectType m SchemaIntrospection)
          !#> const schemaIntrospection
      in
        output metaQuery (Just selectionSet) variables root

    OperationDefinitionNode { operation: Mutation, selectionSet } ->
      maybe
        (throwError $ error "Api does not support mutations.")
        (\mutation -> output mutation (Just selectionSet) variables root)
        s.mutation

    _ -> throwError $ error "Unrecognised operation."

  pure $ serializeResult executionResult

    where
      hasOperationName operationName (OperationDefinitionNode { name: Just (NameNode nameNode) }) =
        nameNode.value == operationName

      hasOperationName _ _ = false