# Scalar Types

## Leaf Types

In GraphQL, Leaf Types are types that turn into a specific value and have no connections to other types (other than their parent node).
There are two GraphQL kinds that can produce leaf types: The Scalar and the Enumeration.
Enum types are used for types that have a finite set of values and serialize by mapping to a finite set of JSON string values.
Scalar types, on the other hand, can serialize to any JSON value.
This includes JSON lists and maps, but usually scalars should use the JSON primitive types for representation: String, Number, Boolean.

## Built in Scalar Types

The GraphQL Specification lists five scalar types, that need to be supported by every GraphQL implementation.
These scalars are probably enough to get you started in your project.

- The `Int` scalar is used for [natural numbers in the Int32 range](http://spec.graphql.org/June2018/#sec-Int).
- The `Float` type represents [signed double‐precision fractional values](http://spec.graphql.org/June2018/#sec-Float).
- The `String` scalar represents [UTF-8 encoded strings](http://spec.graphql.org/June2018/#sec-String).
- The `Boolean` scalar represents [boolean logic truth values](http://spec.graphql.org/June2018/#sec-Boolean).
- And finally the `ID` scalar is used for [unique identifiers](http://spec.graphql.org/June2018/#sec-ID). It is always serialised as a JSON string, but can also accept integer values from client queries.

PureScript GraphQL provides implementations for these scalars in the `GraphQL.Builtin.Scalar` module.
As GraphQL types are values in PureScript, they use lower case identifiers: `int`, `float`, `string`, `boolean` and `id`.
They are exported as well from the `GraphQL` main module.

## Defining custom Scalar Types

Sometimes, the built in scalar types are not enough and you want to represent a custom type together with input validation.
One great example is the `DateTime` scalar.

> **Note:**
> You can find the full code for this example [here](https://github.com/hendrikniemann/purescript-graphql-fullstack-example/blob/master/backend/src/Schema/DateTime.purs).

The predefined scalars are using the `ScalarType` newtype to implement the serialization and deserialization logic.
We can do the same to provide our own custom scalars.
If we want to build a scalar that serializes `DateTime` from `Data.DateTime` into a string in ISO 8601 format (`YYYY-MM-DDTHH:mm:ss.SSSZ`), we have to provide two functions:

- `parseDateTime` which takes a string and returns either an error message or a valid `DateTime`.
- `formatDateTime` which turns a `DateTime` into a string.

Using `Data.Formatter.DateTime`, we can easily write these functions:

```purescript
parseDateTime :: String -> Either String DateTime
parseDateTime = unformat dateFormat

formatDateTime :: DateTime -> String
formatDateTime = format dateFormat

dateFormat ∷ Formatter
dateFormat = parseFormatString "YYYY-MM-DDTHH:mm:ss.SSSZ" # unsafePartial fromRight
```

With these two helper functions we can now implement the functions needed by the scalar:

### `parseLiteral`

`parseLiteral` is called when the scalar is used in an argument in a query and the value is supplied inline as a literal.
Here we have to deal with the different literal types that exist in GraphQL.
In our case we only want to accept `StringValueNode`s and error on everything else.
We can then parse the value using the `parseDateTime` function above.

```purescript
parseLiteral (AST.StringValueNode { value }) = parseDateTime value
parseLiteral _ = Left "Expected string literal node for input type date time."
```

> Usually types only work for a single type of literal but it is totally possible to parse different types of literals if you want to do so.
> It can also be an opportunity to implement more specific error messages.

### `parseValue`

`parseValue` is called whenever a variable of our scalar type is supplied.
In this case we have to react to a JSON value. This is easy with _Argonaut_:

```purescript
parseValue = Json.caseJsonString (Left "Dates must be supplied as ISO strings.") parseDateTime
```

### `serialize`

In `serialize` we have to do the opposite:
Turn a `DateTime` value into a JSON value.
Again, we are using _Argonaut_ and our helper function `formatDateTime` to compose the final function.

```purescript
serialize = formatDateTime >>> Json.fromString
```

!> Scalars in PureScript are designed to be as typesafe and pure as possible.
Therefore `serialize` is not allowed to have side effects or return an error.
All potential errors must be dealt with in the field resolver.
