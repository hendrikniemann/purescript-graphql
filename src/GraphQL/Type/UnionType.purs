module GraphQL.Type.UnionType where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.List (filter)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Effect.Exception (Error)
import GraphQL.Execution.Result (Result)
import GraphQL.Language.AST as AST
import GraphQL.Language.AST.Util (nameFromNameNode)
import GraphQL.Type.Class (class GraphQLType, class OutputType, ExecutionContext)
import GraphQL.Type.Introspection.Datatypes as IntrospectionTypes


newtype UnionType m a = UnionType (Unit ->
  { name :: String
  , description :: Maybe String
  , typeIntrospections :: Array IntrospectionTypes.TypeIntrospection
  , output :: Maybe AST.SelectionSetNode -> ExecutionContext -> m a -> m Result
  }
)


instance graphqlTypeUnionType :: GraphQLType (UnionType m) where
  introspect (UnionType config) = IntrospectionTypes.UnionTypeIntrospection
    { name: (config unit).name
    , description: (config unit).description
    , possibleTypes: \_ -> (config unit).typeIntrospections
    }


-- Takes a string and only returns part of the selection set that matches the return type
-- TODO: Throw under certain cases, e.g. if there is a field selected in the root that is not
--       `__typename`
filterSelectionSet :: String -> AST.SelectionSetNode -> ExecutionContext -> AST.SelectionSetNode
filterSelectionSet typeName (AST.SelectionSetNode { selections }) ctx =
  AST.SelectionSetNode { selections: filter filterFn selections }
    where
      -- Only the __typename field is allowed on the root selection
      filterFn (AST.FieldNode { name: AST.NameNode { value }}) = value == "__typename"

      -- If we find a fragment we only include it into the selection if it matches the type
      filterFn (AST.FragmentSpreadNode { name: AST.NameNode { value }}) =
        case lookup value ctx.fragments of
          Just (AST.FragmentDefinitionNode { typeCondition: AST.SimpleNamedTypeNode { name }}) ->
            nameFromNameNode name == typeName
          _ -> false

      -- Same thing for inline fragment only here we don't have to lookup the fragment
      filterFn (AST.InlineFragmentNode { typeCondition: AST.SimpleNamedTypeNode { name } }) =
        nameFromNameNode name == typeName

instance outputTypeUnionType :: (MonadError Error m) => OutputType m (UnionType m) where
  output :: forall a. UnionType m a -> Maybe AST.SelectionSetNode -> ExecutionContext -> m a -> m Result
  output (UnionType config) = (config unit).output
