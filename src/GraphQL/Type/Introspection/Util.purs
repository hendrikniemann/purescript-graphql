module GraphQL.Type.Introspection.Util where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, empty, member, singleton)
import GraphQL.Type.Introspection.Datatypes (FieldIntrospection(..), SchemaIntrospection(..), TypeIntrospection(..))


collectTypes :: SchemaIntrospection -> Set TypeIntrospection
collectTypes (SchemaIntrospection { queryType, mutationType }) =
  collectTypes' queryType <> maybe empty collectTypes' mutationType
    where
      collectTypes' :: TypeIntrospection -> Set TypeIntrospection
      -- If the type has no fields we can simply return a set with one element
      collectTypes' introspection@(TypeIntrospection { fields: Nothing }) = singleton introspection

      -- If the type has fields we need to recursively go over these fields and add the type of the
      -- fields as well
      collectTypes' introspection@(TypeIntrospection { fields: Just fields }) =
        foldl collectTypesFromField (singleton introspection) fields

      collectTypesFromField :: Set TypeIntrospection -> FieldIntrospection -> Set TypeIntrospection
      collectTypesFromField set (FieldIntrospection config) =
        let
          resultType = config.type unit
        in
          if member resultType set then set else set <> collectTypes' resultType
