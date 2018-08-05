## Module Data.GraphQL

#### `Schema`

``` purescript
data Schema :: Type
```

#### `GraphQLType`

``` purescript
data GraphQLType :: Type -> Type
```

#### `Field`

``` purescript
data Field :: Type -> Type
```

#### `Argument`

``` purescript
data Argument :: Type -> Type
```

##### Instances
``` purescript
(ConvertDeclArgs decl args) => ConvertDeclArgs (Cons k (Argument a) decl) (Cons k a args)
```

#### `stringScalar`

``` purescript
stringScalar :: GraphQLType String
```

#### `intScalar`

``` purescript
intScalar :: GraphQLType Int
```

#### `floatScalar`

``` purescript
floatScalar :: GraphQLType Number
```

#### `idScalar`

``` purescript
idScalar :: GraphQLType String
```

#### `parse`

``` purescript
parse :: String -> Either Error Document
```

Parse a query string and return a parsed GraphQL document or an error.

#### `validate`

``` purescript
validate :: Schema -> Document -> Array Error
```

Validate the AST of a query against a given schema. Returns an array of
errors. The array is empty if all validation pass. Currently only supports
the default validations from `graphql-js`.

#### `execute`

``` purescript
execute :: Schema -> Document -> Aff Json
```

Asyncroniously executes a query given the GraphQL document and a schema.

#### `schema`

``` purescript
schema :: GraphQLType Unit -> Maybe (GraphQLType Unit) -> Schema
```

Create a schema given a root query object type and a root mutation type.
Schemas don't need a mutation type therefore it is optional.

#### `objectType`

``` purescript
objectType :: forall a r. Homogeneous r (Field a) => String -> Maybe String -> {  | r } -> GraphQLType a
```

Create a new object type with the following properties:
- `name` is the name of the object in the schema
- `descrition` is the description of the type
- `fields` is a record of field definitions

#### `field'`

``` purescript
field' :: forall a b. GraphQLType b -> Maybe String -> (a -> Aff b) -> Field a
```

Create a simple field without arguments using
- `type` the type of the field
- `description` the description of the field
- `resolve` a function resolving the value of the field

*Examples*
``` purescript
hello :: Field Unit
hello = field' stringScalar Nothing \_ -> pure "Hello World"

newtype User = User { name :: String, age :: Int }
name :: Field User
name = field' intScalar (Just "Age of the user") resolve
  where
    resolve (User user) = pure user.name
```

#### `field`

``` purescript
field :: forall a b decl args. ArgDeclarationToArgs decl args => GraphQLType b -> Maybe String -> {  | decl } -> (a -> {  | args } -> Aff b) -> Field a
```

#### `argument`

``` purescript
argument :: forall a. GraphQLType a -> Maybe String -> Argument a
```

#### `ArgDeclarationToArgs`

``` purescript
class ArgDeclarationToArgs (decl :: # Type) (args :: # Type) | decl -> args, args -> decl
```

##### Instances
``` purescript
(RowToList decl ldecl, RowToList args largs, ConvertDeclArgs ldecl largs, ListToRow ldecl decl, ListToRow largs args) => ArgDeclarationToArgs decl args
```

#### `ConvertDeclArgs`

``` purescript
class ConvertDeclArgs (decl :: RowList) (args :: RowList) 
```

##### Instances
``` purescript
ConvertDeclArgs Nil Nil
(ConvertDeclArgs decl args) => ConvertDeclArgs (Cons k (Argument a) decl) (Cons k a args)
```


### Re-exported from Data.GraphQL.Document:

#### `Document`

``` purescript
data Document :: Type
```

##### Instances
``` purescript
Show Document
```

