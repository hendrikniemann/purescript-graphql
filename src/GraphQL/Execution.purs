module GraphQL.Execution (execute) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..), either)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import GraphQL.Language.AST (DefinitionNode(..), DocumentNode(..), OperationTypeNode(..))
import GraphQL.Type (Schema(..), serialize)
import Prelude (($), identity)

execute :: forall a. DocumentNode -> Schema a -> a -> Json
execute (DocumentNode { definitions: Cons query Nil }) (Schema s) root =
  either (\error -> encodeJson (Tuple "error" error : Nil)) identity $ case query of
    OperationDefinitionNode { operation: Query, selectionSet } ->
      serialize s.query (Just selectionSet) root
    _ -> Left "Somehow received non OperationDefinitionNode for operation execution..."
execute _ _ _ = encodeJson (Tuple "error" "Can only execute documents with single query" : Nil)
