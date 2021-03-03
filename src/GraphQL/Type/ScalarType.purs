module GraphQL.Type.ScalarType
  ( ScalarType(..)
  ) where


import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Argonaut (Json)
import Data.Either (Either(..), note)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Effect.Exception (Error)
import GraphQL.Execution.Result (Result(..))
import GraphQL.Language.AST as AST
import GraphQL.Type.Class (class GraphQLType, class OutputType, class InputType)
import GraphQL.Type.Introspection.Datatypes (TypeIntrospection(..))


-- | Scalar types are the leaf nodes of a GraphQL schema. Scalar types can be serialised into JSON
-- | or obtained from a JSON when read from the variables provided by a query. Furthermore values
-- | can be read from literals in a GraphQL document. To create a scalar type a name and optionally
-- | a description need to be provided.
-- | The functions `parseLiteral`, `parseValue` and `serialize` are used to convert between the
-- | GraphQL transport formats and PureScript representations.
-- |
-- | The GraphQL PureScript implementation comes with the five default scalars defined in the
-- | specification.
newtype ScalarType a =
  ScalarType
    { name :: String
    , description :: Maybe String
    , parseLiteral :: AST.ValueNode -> Either String a
    , parseValue :: Json -> Either String a
    , serialize :: a -> Json
    }


instance graphqlTypeScalarType :: GraphQLType ScalarType where
  introspect (ScalarType { name, description }) = ScalarTypeIntrospection { name, description }


instance inputTypeScalarType :: InputType ScalarType where
  input (ScalarType config) (Just node) execCtx = case node of
    (AST.VariableNode { name: AST.NameNode { value }}) -> do
      json <- note ("Required variable `" <> value <> "` was not provided.") $
        lookup value execCtx.variables
      config.parseValue json
    _ -> config.parseLiteral node
  input _ Nothing _ = Left "Must provide value for required scalar."


instance outputTypeScalarType :: (MonadError Error m) => OutputType m ScalarType where
  output (ScalarType s) Nothing _ val = ResultLeaf <$> map s.serialize val
  output _ _ _ _ = pure $ ResultError "Invalid subselection on scalar type!"
