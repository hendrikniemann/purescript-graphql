## Module Data.GraphQL.Execution

#### `execute`

``` purescript
execute :: forall a. Schema a -> Document -> a -> Aff Json
```

Asyncroniously executes a query given the GraphQL document and a schema.


