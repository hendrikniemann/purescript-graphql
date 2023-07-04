module GraphQL.Type.InputObjectType where

import Prelude

import Control.Lazy (class Lazy)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import GraphQL.Language.AST as AST
import GraphQL.Type.Class (class GraphQLType, class InputType, ExecutionContext)
import GraphQL.Type.Introspection.Datatypes as IntrospectionTypes
import Type.Proxy (Proxy)


newtype InputObjectType a = InputObjectType (Unit ->
  { name :: String
  , description :: Maybe String
  , fieldIntrospection :: Array IntrospectionTypes.InputValueIntrospection
  , input :: Maybe AST.ValueNode -> ExecutionContext -> Either String a
  }
)


derive instance newtypeInputObjectType :: Newtype (InputObjectType a) _


instance graphqlTypeInputObjectType :: GraphQLType InputObjectType where
  introspect (InputObjectType config) =
    IntrospectionTypes.InputObjectTypeIntrospection
      { name: (config unit).name
      , description: (config unit).description
      , inputFields: (config unit).fieldIntrospection
      }


instance inputTypeInputObjectType :: InputType InputObjectType where
  input (InputObjectType config) = (config unit).input


instance lazyInputObjectType :: Lazy (InputObjectType a) where
  defer fn = InputObjectType $ \_ -> unwrap (fn unit) unit


newtype InputField l a = InputField
  { name :: Proxy l
  , required :: Boolean
  , introspection :: IntrospectionTypes.InputValueIntrospection
  , input :: Maybe AST.ValueNode -> ExecutionContext -> Either String a
  }
