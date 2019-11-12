module GraphQL.Execution (execute) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Encode ((:=), (~>))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Effect.Exception (Error)
import GraphQL.Execution.Result (serializeResult)
import GraphQL.Language.AST (DefinitionNode(..), DocumentNode(..), OperationTypeNode(..))
import GraphQL.Type (Schema(..), VariableMap, output)

simpleError :: forall m. Applicative m => String -> m Json
simpleError e = pure $ "error" := e ~> jsonEmptyObject

execute :: forall m a. MonadError Error m => DocumentNode -> Schema m a -> VariableMap -> m a -> m Json
execute (DocumentNode { definitions: Cons query Nil }) (Schema s) variables root =
  case query of
    OperationDefinitionNode { operation: Query, selectionSet } ->
      serializeResult <$> output s.query (Just selectionSet) variables root
    _ -> simpleError "Somehow received non OperationDefinitionNode for operation execution..."
execute _ _ _ _ =  simpleError "Can only execute documents with single query"
