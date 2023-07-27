module GraphQL.Type.EnumType
  ( EnumType(..)
  , EnumValue(..)
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut as Json
import Data.Either (Either(..), note)
import Data.List (find)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Effect.Exception (Error, error)
import GraphQL.Execution.Result (Result(..))
import GraphQL.Language.AST as AST
import GraphQL.Type.Class (class GraphQLType, class InputType, class OutputType, ExecutionContext)
import GraphQL.Type.Introspection.Datatypes as IntrospectionTypes


newtype EnumType a =
  EnumType
    { name :: String
    , description :: Maybe String
    , values :: Array (EnumValue a)
    }


newtype EnumValue a =
  EnumValue
    { name :: String
    , description :: Maybe String
    , value :: a
    , isValue :: a -> Boolean }


instance graphqlTypeEnumType :: GraphQLType EnumType where
  introspect (EnumType { name, description, values }) =
    IntrospectionTypes.EnumTypeIntrospection
      { name
      , description
      , enumValues: values <#> \(EnumValue val) ->
          IntrospectionTypes.EnumValueIntrospection
            { name: val.name
            , description: val.description
            , deprecationReason: Nothing
            }
      }


instance inputTypeEnumType :: InputType EnumType where
  input (EnumType config) (Just node) execContext =
    let lookupName name =
          note ("Unknown enum value `" <> name <> "` for type `" <> config.name <> "`.") $
            find (\(EnumValue val) -> val.name == name) config.values
    in case node of
      AST.VariableNode { name: AST.NameNode { value: name } } -> do
        json <- note ("Required variable `" <> name <> "` was not provided.") $
          lookup name execContext.variables
        EnumValue val <- Json.caseJsonString (Left "Enum values must be strings.") lookupName json
        pure val.value

      AST.EnumValueNode { name: AST.NameNode { value: name } } -> do
        EnumValue val <- lookupName name
        pure val.value

      _ -> Left "Unexpected non-enum value node for enum value."

  input _ _ _ = Left "Missing value for required argument."

  showDefaultValue (EnumType config) value =
    case find (\(EnumValue { isValue }) -> isValue value) config.values of
      Nothing -> "Unknown"
      Just (EnumValue val) -> val.name


instance outputTypeEnumType :: MonadError Error m => OutputType m EnumType where
  output :: forall a. EnumType a -> Maybe AST.SelectionSetNode -> ExecutionContext -> m a -> m Result
  output (EnumType config) Nothing _ mValue = do
    value <- mValue
    case find (\(EnumValue { isValue }) -> isValue value) config.values of
      Nothing ->
        throwError $ error "Could not select enum value representation from provided value."

      Just (EnumValue val) ->
        pure $ ResultLeaf $ Json.fromString val.name

  output _ _ _ _ = pure $ ResultError "Invalid subselection on enum type!"
