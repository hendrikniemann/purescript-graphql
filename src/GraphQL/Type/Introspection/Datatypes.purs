module GraphQL.Type.Introspection.Datatypes where

import Prelude

import Data.Enum (class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericPred, genericSucc)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

newtype SchemaIntrospection = SchemaIntrospection
  { queryType :: TypeIntrospection
  , mutationType :: Maybe TypeIntrospection
  }


derive instance newtypeSchemaIntrospection :: Newtype SchemaIntrospection _


-- | This data type is used to gather information at runtime about the schema. Each GraphQL type can
-- | be introspected and has to return some information about its structure.
data TypeIntrospection
  = ScalarTypeIntrospection
      { name :: String
      , description :: Maybe String
      }
  | ObjectTypeIntrospection
      { name :: String
      , description :: Maybe String
      , fields :: Array FieldIntrospection
      }
  | EnumTypeIntrospection
      { name :: String
      , description :: Maybe String
      , enumValues :: Array EnumValueIntrospection
      }
  | InputObjectTypeIntrospection
      { name :: String
      , description :: Maybe String
      , inputFields :: Array InputValueIntrospection
      }
  | UnionTypeIntrospection
      { name :: String
      , description :: Maybe String
      , possibleTypes :: Unit -> Array TypeIntrospection
      }
  | ListTypeIntrospection
      { ofType :: Unit -> TypeIntrospection }
  | NonNullTypeIntrospection
      { ofType :: Unit -> TypeIntrospection }


-- | Read the name of a type from a type introspection. Not all types have names: List and
-- | non-null types don't have names on their own.
getName :: TypeIntrospection -> Maybe String
getName (ScalarTypeIntrospection { name }) = Just name
getName (ObjectTypeIntrospection { name }) = Just name
getName (InputObjectTypeIntrospection { name }) = Just name
getName (EnumTypeIntrospection { name }) = Just name
getName (UnionTypeIntrospection { name }) = Just name
getName _ = Nothing

-- | Read the description of a type from a type introspection.
getDescription :: TypeIntrospection -> Maybe String
getDescription (ScalarTypeIntrospection { description }) = description
getDescription (ObjectTypeIntrospection { description }) = description
getDescription (InputObjectTypeIntrospection { description }) = description
getDescription (EnumTypeIntrospection { description }) = description
getDescription (UnionTypeIntrospection { description }) = description
getDescription _ = Nothing


-- | Get the type kind of an introspection
getTypeKind :: TypeIntrospection -> TypeKind
getTypeKind (ScalarTypeIntrospection _) = Scalar
getTypeKind (ObjectTypeIntrospection _) = Object
getTypeKind (InputObjectTypeIntrospection _) = InputObject
getTypeKind (EnumTypeIntrospection _) = Enum
getTypeKind (UnionTypeIntrospection _) = Union
getTypeKind (ListTypeIntrospection _) = List
getTypeKind (NonNullTypeIntrospection _) = NonNull


-- We can abuse the fact that our schema can only have unique type names to create an efficient Eq
-- instance to only compare the names of two types
instance eqTypeIntrospection :: Eq TypeIntrospection where
  eq t1 t2 = getName t1 == getName t2


-- We can abuse the fact that our schema can only have unique type names to create an efficient Ord
-- instance
instance ordTypeIntrospection :: Ord TypeIntrospection where
  compare t1 t2 = compare (getName t1) (getName t2)


newtype EnumValueIntrospection = EnumValueIntrospection
  { name :: String
  , description :: Maybe String
  , deprecationReason :: Maybe String
  }


derive instance newtypeEnumValueIntrospection :: Newtype EnumValueIntrospection _


newtype FieldIntrospection = FieldIntrospection
  { name :: String
  , description :: Maybe String
  , args :: Array InputValueIntrospection
  , type :: Unit -> TypeIntrospection
  , deprecationReason :: Maybe String
  }


derive instance newtypeFieldIntrospection :: Newtype FieldIntrospection _


newtype InputValueIntrospection = InputValueIntrospection
  { name :: String
  , description :: Maybe String
  , type :: Unit -> TypeIntrospection
  , defaultValue :: Maybe String
  }


derive instance newtypeInputValueIntrospection :: Newtype InputValueIntrospection _


-- | An enum value for different GraphQL kinds
data TypeKind
  = Scalar
  | Object
  | Interface
  | Union
  | Enum
  | InputObject
  | List
  | NonNull


derive instance genericTypeKind :: Generic TypeKind _


instance eqTypeKind :: Eq TypeKind where
  eq = genericEq


instance ordTypeKind :: Ord TypeKind where
  compare = genericCompare


instance boundedTypeKind :: Bounded TypeKind where
  top = genericTop
  bottom = genericBottom


instance enumTypeKind :: Enum TypeKind where
  succ = genericSucc
  pred = genericPred


instance showTypeKind :: Show TypeKind where
  show Scalar = "SCALAR"
  show Object = "OBJECT"
  show Interface = "INTERFACE"
  show Union = "UNION"
  show Enum = "ENUM"
  show InputObject = "INPUT_OBJECT"
  show List = "LIST"
  show NonNull = "NON_NULL"
