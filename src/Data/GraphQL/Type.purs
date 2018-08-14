module Data.GraphQL.Type
       ( Schema
       , ObjectType
       , ScalarType
       , ObjectTypeField
       , ObjectTypeFieldArg
       , class OutputType
       , class InputType
       , float
       , id
       , int
       , string
       , schema
       , objectType
       , field
       , field'
       , argument
       , class ArgDeclarationToArgs
       , class ConvertDeclArgs
       ) where

import Prelude

import Prim.RowList (kind RowList, Cons, Nil)
import Control.Promise (Promise, fromAff)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Type.Prelude (class RowToList, class ListToRow)
import Type.Row.Homogeneous (class Homogeneous)

-- | A GraphQL schema containing the root types
foreign import data Schema :: Type -> Type

-- | A GraphQL object type
foreign import data ObjectType :: Type -> Type

-- | A GraphQL scalar type
foreign import data ScalarType :: Type -> Type

-- | A type holding the configuration of a field
foreign import data ObjectTypeField :: Type -> Type

-- | A type holding the configuration of a field argument
foreign import data ObjectTypeFieldArg :: Type -> Type

-- | A type class defining which types are output types
class OutputType a

instance scalarTypeOutputType :: OutputType (ScalarType a)

instance objectTypeOutputType :: OutputType (ObjectType a)

-- | A type class defining which types are input types
class InputType a

instance scalarTypeInputType :: InputType (ScalarType a)

foreign import float :: ScalarType Number

foreign import int :: ScalarType Int

foreign import string :: ScalarType String

foreign import id :: ScalarType String

-- | Create a schema given a root query object type and a root mutation type.
-- | Schemas don't need a mutation type therefore it is optional.
schema :: ∀ a. ObjectType a -> Maybe (ObjectType a) -> Schema a
schema query mutation = runFn2 _schema query $ toNullable mutation

-- | Create a new object type with the following properties:
-- | - `name` is the name of the object in the schema
-- | - `description` is the description of the type
-- | - `fields` is a record of field definitions
objectType :: ∀ a r. Homogeneous r (ObjectTypeField a)
  => String
  -> Maybe String
  -> Record r
  -> ObjectType a
objectType name description =
    runFn3 _objectType name $ toNullable description

-- | Create a simple field without arguments using
-- | - `type` the type of the field
-- | - `description` the description of the field
-- | - `resolve` a function resolving the value of the field
-- |
-- | *Examples*
-- | ``` purescript
-- | hello :: Field Unit
-- | hello = field' stringScalar Nothing \_ -> pure "Hello World"
-- |
-- | newtype User = User { name :: String, age :: Int }
-- | name :: Field User
-- | name = field' intScalar (Just "Age of the user") resolve
-- |   where
-- |     resolve (User user) = pure user.age
-- | ```
field' :: ∀ t a b . OutputType (t b)
  => t b
  -> Maybe String
  -> (a -> Aff b)
  -> ObjectTypeField a
field' t description resolve =
    runFn4 _field t (toNullable description) {} $
      \parent _ -> fromAff $ resolve parent

-- | Create a field with the specified arguments
-- | 
field :: ∀ t a b decl args. OutputType (t b)
  => ArgDeclarationToArgs decl args
  => t b
  -> Maybe String
  -> Record decl
  -> (a -> Record args -> Aff b)
  -> ObjectTypeField a
field t description args resolve =
  runFn4 _field t (toNullable description) args $
      \parent a -> fromAff $ resolve parent a

-- | Create a single argument that can be used by a 
argument :: ∀ t a. InputType (t a)
  => t a
  -> Maybe String
  -> ObjectTypeFieldArg a
argument t description = runFn2 _argument t $ toNullable description

-- | A type class constraining the resolver arguments parameter to the supplied
-- | arguments declaration.
-- | E.g. if the provided args are of type `{ name: Argument String }` the
-- | resolvers second argument needs to be of type `{ name: String }`.
class ArgDeclarationToArgs
  (decl :: # Type)
  (args :: # Type)
  | decl -> args, args -> decl

instance argDeclarationToArgsImpl
  :: ( RowToList decl ldecl
     , RowToList args largs
     , ConvertDeclArgs ldecl largs
     , ListToRow ldecl decl
     , ListToRow largs args )
  => ArgDeclarationToArgs decl args

class ConvertDeclArgs (decl :: RowList) (args :: RowList)

instance convertDeclArgsNil :: ConvertDeclArgs Nil Nil

instance convertDeclArgsCons :: ConvertDeclArgs decl args
  => ConvertDeclArgs (Cons k (ObjectTypeFieldArg a) decl) (Cons k a args)

foreign import _objectType :: ∀ a r.
  Fn3 String (Nullable String) (Record r) (ObjectType a)

foreign import _schema :: ∀ a.
  Fn2 (ObjectType a) (Nullable (ObjectType a)) (Schema a)

foreign import _field :: ∀ t a b decl args.
  Fn4
  (t b)
  (Nullable String)
  decl
  (a -> args -> Effect (Promise b))
  (ObjectTypeField a)

foreign import _argument :: ∀ t a.
  Fn2 (t a) (Nullable String) (ObjectTypeFieldArg a)
