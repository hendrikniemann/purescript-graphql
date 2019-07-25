module GraphQL.Execution (execute) where

import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Encode ((:=), (~>))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import GraphQL.Language.AST (DefinitionNode(..), DocumentNode(..), OperationTypeNode(..))
import GraphQL.Execution.Result (serializeResult)
import GraphQL.Type (Schema(..), VariableMap, output)
import Prelude (($))

simpleError :: String -> Json
simpleError e = "error" := e ~> jsonEmptyObject

execute :: forall a. DocumentNode -> Schema a -> VariableMap -> a -> Json
execute (DocumentNode { definitions: Cons query Nil }) (Schema s) variables root =
  case query of
    OperationDefinitionNode { operation: Query, selectionSet } ->
      serializeResult $ output s.query (Just selectionSet) variables root
    _ -> simpleError "Somehow received non OperationDefinitionNode for operation execution..."
execute _ _ _ _ = simpleError "Can only execute documents with single query"
