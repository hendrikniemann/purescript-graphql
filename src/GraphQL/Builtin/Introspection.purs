module GraphQL.Builtin.Introspection where

import Prelude

import Control.Lazy (defer)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect.Exception (Error)
import GraphQL.Builtin.Scalar as Scalar
import GraphQL.DSL ((!#>), (.>), (:>))
import GraphQL.DSL (enumType, field, listField, nullableField, nullableListField, objectType) as GraphQL
import GraphQL.OptionallyParallel (class OptionallyParallel)
import GraphQL.Type (ObjectType, EnumType) as GraphQL
import GraphQL.Type.Introspection.Datatypes (EnumValueIntrospection, FieldIntrospection, InputValueIntrospection, SchemaIntrospection, TypeIntrospection(..), TypeKind, getDescription, getName, getTypeKind)
import GraphQL.Type.Introspection.Util (collectTypes)


schemaType :: forall f m. (MonadError Error m) => (OptionallyParallel f m) => GraphQL.ObjectType m SchemaIntrospection
schemaType = GraphQL.objectType "__Schema"
  :> """
    A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available
    types and directives on the server, as well as the entry points for query, mutation, and
    subscription operations.
  """
  .> GraphQL.listField "types" (typeType :: GraphQL.ObjectType m TypeIntrospection)
    :> "A list of all types supported by this server."
    !#> collectTypes >>> fromFoldable
  .> GraphQL.field "queryType" (typeType :: GraphQL.ObjectType m TypeIntrospection)
    :> "The type that query operations will be rooted at."
    !#> unwrap >>> _.queryType
  .> GraphQL.nullableField "mutationType" (typeType :: GraphQL.ObjectType m TypeIntrospection)
    :> """
      If this server supports mutation, the type that mutation operations will be rooted at.
    """
    !#> unwrap >>> _.mutationType
  .> GraphQL.nullableField "subscriptionType" (typeType :: GraphQL.ObjectType m TypeIntrospection)
    :> """
      If this server supports subscriptons, the type that subscription operations will be rooted
      at.
    """
    !#> const Nothing
  .> GraphQL.listField "directives" Scalar.string
    !#> const []


typeType :: forall f m. (MonadError Error m) => (OptionallyParallel f m) => GraphQL.ObjectType m TypeIntrospection
typeType = typeType'
  where
  typeType' = GraphQL.objectType "__Type"
    :> """
      The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in
      GraphQL as represented by the `__TypeKind` enum.\n\nDepending on the kind of a type, certain
      fields describe information about that type. Scalar types provide no information beyond a
      name and description, while Enum types provide their values. Object and Interface types
      provide the fields they describe. Abstract types, Union and Interface, provide the Object
      types possible at runtime. List and NonNull types compose other types.
    """
    .> GraphQL.field "kind" typeKindType
      !#> getTypeKind
    .> GraphQL.nullableField "name" Scalar.string
      !#> getName
    .> GraphQL.nullableField "description" Scalar.string
      !#> getDescription
    .> GraphQL.nullableListField "fields" (defer \_ -> fieldType)
      !#> case _ of
            (ObjectTypeIntrospection { fields }) -> Just fields
            _ -> Nothing
    -- For spec compliance; this implementation does not support interfaces yet
    .> GraphQL.nullableListField "interfaces" (defer \_ -> typeType')
      !#> case _ of
            (ObjectTypeIntrospection _) -> Just []
            _ -> Nothing
    .> GraphQL.nullableListField
          "possibleTypes"
          (defer \_ -> typeType')
      !#> case _ of
            (UnionTypeIntrospection { possibleTypes }) -> Just $ possibleTypes unit
            _ -> Nothing
    .> GraphQL.nullableListField "enumValues" enumValueType
      !#> case _ of
            (EnumTypeIntrospection { enumValues }) -> Just enumValues
            _ -> Nothing
    .> GraphQL.nullableListField "inputFields" (defer \_ -> inputValueType)
      !#> case _ of
            (InputObjectTypeIntrospection { inputFields }) -> Just inputFields
            _ -> Nothing
    .> GraphQL.nullableField "ofType" (defer \_ -> typeType')
      !#> case _ of
            (ListTypeIntrospection { ofType }) -> Just (ofType unit)
            (NonNullTypeIntrospection { ofType }) -> Just (ofType unit)
            _ -> Nothing

  typeKindType :: GraphQL.EnumType TypeKind
  typeKindType = GraphQL.enumType "__TypeKind"
    :> "An enum describing what kind of type a given `__Type` is."

  fieldType :: GraphQL.ObjectType m FieldIntrospection
  fieldType = GraphQL.objectType "__Field"
    :> """
      Object and Interface types are described by a list of Fields, each of which has a name,
      potentially a list of arguments, and a return type.
    """
    .> GraphQL.field "name" Scalar.string
      !#> unwrap >>> _.name
    .> GraphQL.nullableField "description" Scalar.string
      !#> unwrap >>> _.description
    .> GraphQL.field "type" (defer \_ -> typeType')
      !#> unwrap >>> _.type >>> (_ $ unit)
    .> GraphQL.listField "args" (defer \_ -> inputValueType)
      !#> unwrap >>> _.args
    .> GraphQL.field "isDeprecated" Scalar.boolean
      !#> unwrap >>> _.deprecationReason >>> isJust
    .> GraphQL.nullableField "deprecationReason" Scalar.string
      !#> unwrap >>> _.deprecationReason

  enumValueType :: GraphQL.ObjectType m EnumValueIntrospection
  enumValueType = GraphQL.objectType "__EnumValue"
    :> """
      One possible value for a given Enum. Enum values are unique values, not a placeholder for a
      string or numeric value. However an Enum value is returned in a JSON response as a string.
    """
    .> GraphQL.field "name" Scalar.string
      !#> unwrap >>> _.name
    .> GraphQL.nullableField "description" Scalar.string
      !#> unwrap >>> _.description
    .> GraphQL.field "isDeprecated" Scalar.boolean
      !#> unwrap >>> _.deprecationReason >>> isJust
    .> GraphQL.nullableField "deprecationReason" Scalar.string
      !#> unwrap >>> _.deprecationReason

  inputValueType :: GraphQL.ObjectType m InputValueIntrospection
  inputValueType = GraphQL.objectType "__InputValue"
    :> """
      Arguments provided to Fields or Directives and the input fields of an InputObject are
      represented as Input Values which describe their type and optionally a default value.
    """
    .> GraphQL.field "name" Scalar.string
      !#> unwrap >>> _.name
    .> GraphQL.nullableField "description" Scalar.string
      !#> unwrap >>> _.description
    .> GraphQL.field "type" (defer \_ -> typeType')
      !#> unwrap >>> _.type >>> (_ $ unit)
    .> GraphQL.nullableField "defaultValue" Scalar.string
      !#> unwrap >>> _.defaultValue


-- type __Directive {
--   name: String!
--   description: String
--   locations: [__DirectiveLocation!]!
--   args: [__InputValue!]!
-- }

-- enum __DirectiveLocation {
--   QUERY
--   MUTATION
--   SUBSCRIPTION
--   FIELD
--   FRAGMENT_DEFINITION
--   FRAGMENT_SPREAD
--   INLINE_FRAGMENT
--   SCHEMA
--   SCALAR
--   OBJECT
--   FIELD_DEFINITION
--   ARGUMENT_DEFINITION
--   INTERFACE
--   UNION
--   ENUM
--   ENUM_VALUE
--   INPUT_OBJECT
--   INPUT_FIELD_DEFINITION
-- }
