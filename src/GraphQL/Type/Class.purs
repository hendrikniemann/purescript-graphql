module GraphQL.Type.Class
  ( class GraphQLType
  , introspect
  , class InputType
  , input
  , showDefaultValue
  , class OutputType
  , output
  , ExecutionContext
  ) where

import Control.Monad.Error.Class (class MonadError)
import Data.Argonaut (Json)
import Data.Either (Either)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect.Exception (Error)
import GraphQL.Execution.Result (Result)
import GraphQL.Language.AST as AST
import GraphQL.Type.Introspection.Datatypes as IntrospectionTypes

type ExecutionContext =
  { variables :: Map String Json
  , fragments :: Map String AST.DefinitionNode
  }

-- | The type class for all GraphQL types
class GraphQLType :: forall k. (k -> Type) -> Constraint
class GraphQLType t where
  introspect :: forall a. t a -> IntrospectionTypes.TypeIntrospection

-- | The input type class is used for all GraphQL types that can be used as input types.
class GraphQLType t <= InputType t where
  input :: forall a. t a -> Maybe AST.ValueNode -> ExecutionContext -> Either String a
  showDefaultValue :: forall a. t a -> a -> String

-- | The output type class is used for all GraphQL types that can be used as output types.
-- | It has instances for GraphQL types that can resolve a value within a certain monad error.
class (MonadError Error m, GraphQLType t) <= OutputType m t where
  output :: forall a. t a -> Maybe AST.SelectionSetNode -> ExecutionContext -> m a -> m Result
