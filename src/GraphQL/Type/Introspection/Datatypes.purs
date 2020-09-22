module GraphQL.Type.Introspection.Datatypes where

import Prelude

import Data.Enum (class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericPred, genericSucc)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

newtype SchemaIntrospection = SchemaIntrospection
  { queryType :: TypeIntrospection
  , mutationType :: Maybe TypeIntrospection
  }


derive instance newtypeSchemaIntrospection :: Newtype SchemaIntrospection _


newtype TypeIntrospection = TypeIntrospection
  { kind :: TypeKind
  , name :: Maybe String
  , description :: Maybe String
  , fields :: Maybe (Array FieldIntrospection)
  , enumValues :: Maybe (Array EnumValueIntrospection)
  , inputs :: Maybe (Array InputValueIntrospection)
  , possibleTypes :: Maybe (Unit -> Array TypeIntrospection)
  , ofType :: Maybe (Unit -> TypeIntrospection)
  }


derive instance newtypeTypeIntrospection :: Newtype TypeIntrospection _


-- We can abuse the fact that our schema can only have unique type names to create an efficient Eq
-- instance
instance eqTypeIntrospection :: Eq TypeIntrospection where
  eq (TypeIntrospection t1) (TypeIntrospection t2) = t1.name == t2.name


-- We can abuse the fact that our schema can only have unique type names to create an efficient Ord
-- instance
instance ordTypeIntrospection :: Ord TypeIntrospection where
  compare (TypeIntrospection t1) (TypeIntrospection t2) = compare t1.name t2.name


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
