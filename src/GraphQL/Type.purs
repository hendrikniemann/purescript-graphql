module GraphQL.Type
       ( Schema
       , EnumType
       , EnumValue
       , ObjectType
       , ObjectTypeField
       , ObjectTypeFieldArg
       , ScalarType
       , ListType
       , InputObjectType
       , InputObjectTypeField
       , class GraphQLType
       , class OutputType
       , class InputType
       , float
       , id
       , int
       , string
       , list
       , nonNull
       , schema
       , enumType
       , enumValue
       , objectType
       , field
       , field'
       , argument
       , inputObjectType
       , inputField
       , class ArgDeclarationToArgs
       , class ConvertDeclArgs
       , class InputFieldsToReturnType
       , class ConvertInputReturn
       ) where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Array (fromFoldable)
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Prim.RowList (kind RowList, Cons, Nil)
import Prim.TypeError (class Fail, class Warn, Text, Above, Beside, Quote)
import Type.Prelude (class RowToList, class ListToRow)
import Type.Row.Homogeneous (class Homogeneous)

-- | A GraphQL schema with types for context and root
foreign import data Schema :: Type -> Type -> Type

-- | A GraphQL object type
foreign import data ObjectType :: Type -> Type -> Type

-- | A GraphQL scalar type
foreign import data ScalarType :: Type -> Type

-- | A GraphQL enum type
foreign import data EnumType :: Type -> Type

-- | The configuration of a field represented in native JavaScript
foreign import data ObjectTypeField :: Type -> Type -> Type

-- | The configuration of a field argument represented in native JavaScript
foreign import data ObjectTypeFieldArg :: Type -> Type

-- | A GraphQL list type
foreign import data ListType :: Type -> Type

-- | An input object type to create complex input objects
foreign import data InputObjectType :: Type -> Type

-- | The configuration of a field represented in native JavaScript
foreign import data InputObjectTypeField :: Type -> Type

-- | A type class for all GraphQL types.
class GraphQLType a

instance scalarTypeGraphQLType :: GraphQLType (ScalarType a)

instance enumTypeGraphQLType :: GraphQLType (EnumType a)

instance objectTypeGraphQLType :: GraphQLType (ObjectType ctx a)

instance listTypeGraphQLType :: GraphQLType (ListType a)

instance inputObjectTypeGraphQLType :: GraphQLType (InputObjectType a)

-- | A type class defining which types are output types and at the same time
-- | ensuring that the specific output type is bound to a certain context type.
class (GraphQLType a) <= OutputType a ctx

instance scalarTypeOutputType :: OutputType (ScalarType a) ctx

instance enumTypeOutputType :: OutputType (EnumType a) ctx

instance objectTypeOutputType :: OutputType (ObjectType ctx a) ctx
else instance objectTypeOutputTypeFail
  :: Fail (Above
    (Text "Cannot use object type with different context as field result.")
    (Above
      (Beside (Text "Expected context type: ") (Quote expected))
      (Beside (Text "But received constext type: ") (Quote actual))))
  => OutputType (ObjectType actual a) expected

instance listTypeOutputType
  :: (OutputType a ctx)
  => OutputType (ListType (Array a)) ctx

-- | A type class defining which types are input types
class (GraphQLType a) <= InputType a

instance scalarTypeInputType :: InputType (ScalarType a)

instance listTypeInputType :: (InputType a) => InputType (ListType (Array a))

instance enumTypeInputType :: InputType (EnumType a)

instance inputObjectTypeInputType :: InputType (InputObjectType a)

foreign import float :: ScalarType (Maybe Number)

foreign import int :: ScalarType (Maybe Int)

foreign import string :: ScalarType (Maybe String)

foreign import id :: ScalarType (Maybe String)

foreign import list :: ∀ t a. GraphQLType (t a) => t a -> ListType (Maybe (Array a))

foreign import nonNull :: ∀ t a. GraphQLType (t (Maybe a)) => t (Maybe a) -> t a

-- | Create a schema given a root query object type and a root mutation type.
-- | Schemas don't need a mutation type therefore it is optional.
schema :: ∀ a ctx.
  ObjectType ctx (Maybe a) -> Maybe (ObjectType ctx (Maybe a)) -> Schema ctx a
schema query mutation = runFn2 _schema query $ toNullable mutation

-- | A type to store enum value configurations in
newtype EnumValue a = EnumValue
    { name :: String
    , description :: Nullable String
    , value :: a
    }

-- | Create a new enum type. The enum type takes a name, an optional description
-- | and a Folable of enum values.
-- | _Example:_
-- | ```purescript
-- | data Status = Processing | Ready
-- |
-- | statusType :: EnumType Status
-- | statusType = enumType
-- |   "Status"
-- |   (Just "This type indicates the status of a request.")
-- |   [ enumValue "PROCESSING" (Just "In the process.") Processing
-- |   , enumValue "READY" (Just "Completed and ready.") Ready
-- |   ]
-- | ```
enumType :: ∀ f a. (Foldable f)
  => String -> Maybe String -> f (EnumValue a) -> EnumType (Maybe a)
enumType name description values =
  runFn3 _enumType name (toNullable description) (fromFoldable values)

-- | Create a new enum value to use in an enum type definition
enumValue :: ∀ a. String -> Maybe String -> a -> EnumValue a
enumValue name description value =
  EnumValue { name, description: toNullable description, value }

-- | Create a new object type with the following properties:
-- | - `name` is the name of the object in the schema
-- | - `description` is the description of the type
-- | - `fields` is a record of field definitions
objectType :: ∀ a ctx fields. Homogeneous fields (ObjectTypeField ctx a)
  => String
  -> Maybe String
  -> Record fields
  -> ObjectType ctx (Maybe a)
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
-- | hello = field' string Nothing \_ -> pure "Hello World"
-- |
-- | type User = { name :: String, age :: Int }
-- | age :: Field User
-- | age = field' intScalar (Just "Age of the user") resolve
-- |   where
-- |     resolve user = pure user.age
-- | ```
field' :: ∀ t a b ctx. OutputType (t b) ctx
  => t b
  -> Maybe String
  -> (a -> ctx -> Aff b)
  -> ObjectTypeField ctx a
field' t description resolve =
  runFn4 boundField t (toNullable description) {} $
    \parent _ ctx -> fromAff $ resolve parent ctx

-- | Create a field with the specified arguments
-- | 
field :: ∀ t a b ctx decl args. OutputType (t b) ctx
  => ArgDeclarationToArgs decl args
  => t b
  -> Maybe String
  -> Record decl
  -> (a -> Record args -> ctx -> Aff b)
  -> ObjectTypeField ctx a
field t description args resolve =
  runFn4 boundField t (toNullable description) args $
    \parent a ctx -> fromAff $ resolve parent a ctx

-- | Create a single argument that can be used inside an argument declaration
argument :: ∀ t a. InputType (t a)
  => t a
  -> Maybe String
  -> ObjectTypeFieldArg a
argument t description = runFn2 _argument t $ toNullable description

inputObjectType :: ∀ a r. InputFieldsToReturnType r a
  => String
  -> Maybe String
  -> Record r
  -> InputObjectType (Maybe (Record a))
inputObjectType name description fields =
  runFn3 _inputObjectType name (toNullable description) fields

inputField :: ∀ t a. InputType (t a)
  => (t a)
  -> (Maybe String)
  -> (InputObjectTypeField a)
inputField t description = runFn2 _inputField t (toNullable description)

class InputFieldsToReturnType
  (input :: # Type)
  (return :: # Type)
  | input -> return

instance inputFieldToReturnTypeImpl
  :: ( RowToList input linput
     , RowToList return lreturn
     , ConvertInputReturn linput lreturn
     , ListToRow linput input
     , ListToRow lreturn return)
  => InputFieldsToReturnType input return

class ConvertInputReturn (linput :: RowList) (lreturn :: RowList)

instance convertInputReturnNil :: ConvertInputReturn Nil Nil

instance convertInputReturnCons :: ConvertInputReturn linput lreturn
  => ConvertInputReturn (Cons k (InputObjectTypeField a) linput) (Cons k a lreturn)

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
     , ConvertDeclArgs ldecl largs
     , ListToRow largs args )
  => ArgDeclarationToArgs decl args

class ConvertDeclArgs (ldecl :: RowList) (largs :: RowList)

instance convertDeclArgsNil :: ConvertDeclArgs Nil Nil

instance convertDeclArgsCons :: ConvertDeclArgs ldecl largs
  => ConvertDeclArgs (Cons k (ObjectTypeFieldArg a) ldecl) (Cons k a largs)

-- The function can also consume less arguments than specified
else instance convertDeclArgsConsWarn
  :: Warn (Beside
    (Text "Field defines argument ")
    (Beside (Text k) (Text " but resolver does not use the argument.")))
  => ConvertDeclArgs (Cons k (ObjectTypeFieldArg a) taildecl) Nil

-- But we cannot use arguments that don't exist in the declaration
else instance convertDeclArgsConsFail
  :: Fail (Above
    (Beside
      (Beside (Text "Reolver function expects argument ") (Text k))
      (Beside (Text " of type ") (Quote a)))
    (Text "But the argument declaration does not contain this argument."))
  => ConvertDeclArgs Nil (Cons k a largs)

foreign import _schema :: ∀ a ctx.
  Fn2
    (ObjectType ctx (Maybe a))
    (Nullable (ObjectType ctx (Maybe a)))
    (Schema ctx a)

foreign import _enumType :: ∀ a.
  Fn3 String (Nullable String) (Array (EnumValue a)) (EnumType (Maybe a))

foreign import _objectType :: ∀ a ctx fields.
  Fn3 String (Nullable String) (Record fields) (ObjectType ctx a)

boundField :: ∀ t a b decl args ctx.
  Fn4
    (t b)
    (Nullable String)
    decl
    (a -> args -> ctx -> Effect (Promise b))
    (ObjectTypeField ctx a)
boundField = runFn2 _field toNullable toMaybe

foreign import _field :: ∀ z t a b decl args ctx.
  Fn2
    (Maybe z -> Nullable z)
    (Nullable z -> Maybe z)
    (Fn4
      (t b)
      (Nullable String)
      decl
      (a -> args -> ctx -> Effect (Promise b))
      (ObjectTypeField ctx a)
    )

foreign import _argument :: ∀ t a.
  Fn2 (t a) (Nullable String) (ObjectTypeFieldArg a)

foreign import _inputObjectType :: ∀ a r.
  Fn3 String (Nullable String) (Record r) (InputObjectType (Maybe a))

foreign import _inputField :: ∀ t a.
  Fn2 (t a) (Nullable String) (InputObjectTypeField a)
