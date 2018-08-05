module Data.GraphQL
       ( Schema
       , GraphQLType
       , Field
       , Argument
       , stringScalar
       , intScalar
       , floatScalar
       , idScalar
       , parse
       , validate
       , execute
       , schema
       , objectType
       , field'
       , field
       , argument
       , class ArgDeclarationToArgs
       , class ConvertDeclArgs
       , module Data.GraphQL.Document
       ) where

import Prelude

import Prim.RowList (kind RowList, Cons, Nil)
import Control.Promise (Promise, fromAff)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)
import Data.GraphQL.Document (Document)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Type.Prelude (class RowToList, class ListToRow)
import Type.Row.Homogeneous (class Homogeneous)

foreign import data Schema :: Type
foreign import data GraphQLType :: Type -> Type
foreign import data Field :: Type -> Type
foreign import data Argument :: Type -> Type

foreign import stringScalar :: GraphQLType String
foreign import intScalar :: GraphQLType Int
foreign import floatScalar :: GraphQLType Number
foreign import idScalar :: GraphQLType String

-- | Parse a query string and return a parsed GraphQL document or an error.
parse :: String -> Either Error Document
parse = runFn3 nativeParse Left Right

-- | Validate the AST of a query against a given schema. Returns an array of
-- | errors. The array is empty if all validation pass. Currently only supports
-- | the default validations from `graphql-js`.
validate :: Schema -> Document -> Array Error
validate = runFn2 nativeValidate

-- | Asyncroniously executes a query given the GraphQL document and a schema.
execute :: Schema -> Document -> Aff Json
execute scm document = fromEffectFnAff $ runFn2 nativeExecute scm document

-- | Create a schema given a root query object type and a root mutation type.
-- | Schemas don't need a mutation type therefore it is optional.
schema :: GraphQLType Unit -> Maybe (GraphQLType Unit) -> Schema
schema query mutation = runFn2 nativeSchema query $ toNullable mutation

-- | Create a new object type with the following properties:
-- | - `name` is the name of the object in the schema
-- | - `descrition` is the description of the type
-- | - `fields` is a record of field definitions
objectType :: ∀ a r. Homogeneous r (Field a)
  => String
  -> Maybe String
  -> Record r
  -> GraphQLType a
objectType name description =
    runFn3 nativeObjectType name $ toNullable description

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
-- |     resolve (User user) = pure user.name
-- | ```
field' :: ∀ a b . GraphQLType b -> Maybe String -> (a -> Aff b) -> Field a
field' t description resolve =
    runFn4 nativeField t (toNullable description) {} $
      \parent _ -> fromAff $ resolve parent

field :: ∀ a b decl args. ArgDeclarationToArgs decl args
  => GraphQLType b
  -> Maybe String
  -> Record decl
  -> (a -> Record args -> Aff b)
  -> Field a
field t description args resolve =
  runFn4 nativeField t (toNullable description) args $
      \parent a -> fromAff $ resolve parent a

argument :: ∀ a . GraphQLType a -> Maybe String -> Argument a
argument t description = runFn2 nativeArgument t $ toNullable description

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
  => ConvertDeclArgs (Cons k (Argument a) decl) (Cons k a args)

foreign import nativeParse :: ∀ a b. Fn3
  (a -> Either a b)
  (b -> Either a b)
  String
  (Either Error Document)

foreign import nativeExecute ::
  Fn2 Schema Document (EffectFnAff Json)

foreign import nativeValidate :: Fn2 Schema Document (Array Error)

foreign import nativeSchema ::
  Fn2 (GraphQLType Unit) (Nullable (GraphQLType Unit)) Schema

foreign import nativeObjectType :: ∀ a r.
  Fn3 String (Nullable String) (Record r) (GraphQLType a)

foreign import nativeField :: ∀ a b decl args.
  Fn4
  (GraphQLType b)
  (Nullable String)
  decl
  (a -> args -> Effect (Promise b))
  (Field a)

foreign import nativeArgument :: ∀ a.
  Fn2 (GraphQLType a) (Nullable String) (Argument a)
