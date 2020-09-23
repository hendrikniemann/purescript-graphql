module GraphQL.Type.Introspection.Util where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (maybe)
import Data.Set (Set, empty, insert, member)
import GraphQL.Type.Introspection.Datatypes (FieldIntrospection(..), InputValueIntrospection(..), SchemaIntrospection(..), TypeIntrospection(..))


collectTypes :: SchemaIntrospection -> Set TypeIntrospection
collectTypes (SchemaIntrospection { queryType, mutationType }) =
  collectTypesRec queryType empty <> maybe empty (flip collectTypesRec empty) mutationType
    where
      collectTypesRec :: TypeIntrospection -> Set TypeIntrospection -> Set TypeIntrospection
      -- If the type is a wrapper type, we try to insert the inner type
      collectTypesRec (ListTypeIntrospection { ofType }) set = collectTypesRec (ofType unit) set

      collectTypesRec (NonNullTypeIntrospection { ofType }) set = collectTypesRec (ofType unit) set

      -- If the type has fields we need to recursively go over these fields and add the type of the
      -- fields as well
      collectTypesRec introspection@(ObjectTypeIntrospection { fields }) set =
        if member introspection set
        then set
        else foldl collectTypesFromField (insert introspection set) fields

      -- If the type has no fields we can simply return a set with one element
      collectTypesRec introspection set =
        insert introspection set

      collectTypesFromField :: Set TypeIntrospection -> FieldIntrospection -> Set TypeIntrospection
      collectTypesFromField set (FieldIntrospection config) =
        let
          argumentTypes = config.args
          resultType = config.type unit
          newSet = foldl collectTypesFromArg set argumentTypes
        in
          if member resultType newSet then newSet else collectTypesRec resultType newSet

      collectTypesFromArg ::
        Set TypeIntrospection ->
        InputValueIntrospection ->
        Set TypeIntrospection
      collectTypesFromArg set (InputValueIntrospection config) =
        let
          inputType = config.type unit
        in
          if member inputType set then set else
            let
              newSet = case inputType of
                -- We have to recursively add all the input object type's fields as well
                (InputObjectTypeIntrospection { inputFields: fields }) ->
                  foldl collectTypesFromArg set fields

                -- Or if this type is just a trivial input type we can go on with the current set
                _ -> set
            in
              insert inputType newSet
