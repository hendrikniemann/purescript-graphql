## Module Data.GraphQL.Validation

#### `validate`

``` purescript
validate :: forall a. Schema a -> Document -> Array Error
```

Validate the AST of a query against a given schema. Returns an array of
errors. The array is empty if all validation pass. Currently only supports
the default validations from `graphql-js`.


