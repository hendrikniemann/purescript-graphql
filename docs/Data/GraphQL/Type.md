## Module Data.GraphQL.Type

#### `Schema`

``` purescript
data Schema :: Type -> Type
```

A GraphQL schema containing the root types

#### `ObjectType`

``` purescript
data ObjectType :: Type -> Type
```

A GraphQL object type

##### Instances
``` purescript
OutputType (ObjectType a)
```

#### `ScalarType`

``` purescript
data ScalarType :: Type -> Type
```

A GraphQL scalar type

##### Instances
``` purescript
OutputType (ScalarType a)
InputType (ScalarType a)
```

#### `ObjectTypeField`

``` purescript
data ObjectTypeField :: Type -> Type
```

A type holding the configuration of a field

#### `ObjectTypeFieldArg`

``` purescript
data ObjectTypeFieldArg :: Type -> Type
```

A type holding the configuration of a field argument

##### Instances
``` purescript
(ConvertDeclArgs decl args) => ConvertDeclArgs (Cons k (ObjectTypeFieldArg a) decl) (Cons k a args)
```

#### `OutputType`

``` purescript
class OutputType a 
```

A type class defining which types are output types

##### Instances
``` purescript
OutputType (ScalarType a)
OutputType (ObjectType a)
```

#### `InputType`

``` purescript
class InputType a 
```

A type class defining which types are input types

##### Instances
``` purescript
InputType (ScalarType a)
```

#### `floatScalar`

``` purescript
floatScalar :: ScalarType Number
```

#### `idScalar`

``` purescript
idScalar :: ScalarType String
```

#### `intScalar`

``` purescript
intScalar :: ScalarType Int
```

#### `stringScalar`

``` purescript
stringScalar :: ScalarType String
```

#### `schema`

``` purescript
schema :: forall a. ObjectType a -> Maybe (ObjectType a) -> Schema a
```

Create a schema given a root query object type and a root mutation type.
Schemas don't need a mutation type therefore it is optional.

#### `objectType`

``` purescript
objectType :: forall a r. Homogeneous r (ObjectTypeField a) => String -> Maybe String -> {  | r } -> ObjectType a
```

Create a new object type with the following properties:
- `name` is the name of the object in the schema
- `description` is the description of the type
- `fields` is a record of field definitions

#### `field`

``` purescript
field :: forall t a b decl args. OutputType (t b) => ArgDeclarationToArgs decl args => t b -> Maybe String -> {  | decl } -> (a -> {  | args } -> Aff b) -> ObjectTypeField a
```

Create a field with the specified arguments


#### `field'`

``` purescript
field' :: forall t a b. OutputType (t b) => t b -> Maybe String -> (a -> Aff b) -> ObjectTypeField a
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
    resolve (User user) = pure user.age
```

#### `argument`

``` purescript
argument :: forall t a. InputType (t a) => t a -> Maybe String -> ObjectTypeFieldArg a
```

Create a single argument that can be used by a 

#### `ArgDeclarationToArgs`

``` purescript
class ArgDeclarationToArgs (decl :: # Type) (args :: # Type) | decl -> args, args -> decl
```

A type class constraining the resolver arguments parameter to the supplied
arguments declaration.
E.g. if the provided args are of type `{ name: Argument String }` the
resolvers second argument needs to be of type `{ name: String }`.

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
(ConvertDeclArgs decl args) => ConvertDeclArgs (Cons k (ObjectTypeFieldArg a) decl) (Cons k a args)
```


