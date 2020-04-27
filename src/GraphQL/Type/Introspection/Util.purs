module GraphQL.Type.Introspection.Util where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, empty, insert, member)
import GraphQL.Type.Introspection.Datatypes (FieldIntrospection(..), SchemaIntrospection(..), TypeIntrospection(..))


collectTypes :: SchemaIntrospection -> Set TypeIntrospection
collectTypes (SchemaIntrospection { queryType, mutationType }) =
  collectTypesRec queryType empty <> maybe empty (flip collectTypesRec empty) mutationType
    where
      collectTypesRec :: TypeIntrospection -> Set TypeIntrospection -> Set TypeIntrospection
      -- If the type is a wrapper type, we try to insert the inner type
      collectTypesRec introspection@(TypeIntrospection
        { fields: Nothing, ofType: Just t }
      ) set = collectTypesRec (t unit) set

      -- If the type has no fields we can simply return a set with one element
      collectTypesRec introspection@(TypeIntrospection { fields: Nothing }) set =
        insert introspection set

      -- If the type has fields we need to recursively go over these fields and add the type of the
      -- fields as well
      collectTypesRec introspection@(TypeIntrospection { fields: Just fields }) set =
        if member introspection set
        then set
        else foldl collectTypesFromField (insert introspection set) fields

      collectTypesFromField :: Set TypeIntrospection -> FieldIntrospection -> Set TypeIntrospection
      collectTypesFromField set (FieldIntrospection config) =
        let
          resultType = config.type unit
        in
          if member resultType set then set else collectTypesRec resultType set



