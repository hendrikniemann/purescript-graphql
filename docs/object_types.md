# Obejct Types

## About object types

Object types are used to represent complex data structures.
Objects map very closely to records or structures in other programming languages.
In SDL, object types are defined in a way that is very similar to PureScript records or TypeScript objects:

```graphql
type Todo {
  id: ID!
  title: String!
  createdAt: DateTime!
  completedAt: DateTime
}
```

An object type on the API level describes an offer to the client.
Which fields are returned to the client depends on the query.

## Creating an object type

Imagine we want to build a simple object type for the following record type in PureScript:

```purescript
type Todo =
  { id :: Int
  , title :: String
  , createdAt :: DateTime
  , completedAt :: Maybe DateTime
  }
```

We start with an empty object type by giving it a name:

```purescript
todoType :: GraphQL.ObjectType Context Todo
todoType = GraphQL.objectType "Todo"
```

In the type signature, we define the context in which our GraphQL operations are executed.
This can be any monad with a `MonadError Error` instance, like `Aff`, `Effect`, `Either Error`, etc.
Obviously, if we want to run side effects, we need an appropriate monad.
I recommend to create a fitting monad transformer stack and exporting the type to your object types.
For now we can use `Effect`, which allows us to run simple effects like logging.

The second type is the root value (or sometimes called parent value) type.
In this case, we want to build an object type for the `Todo` type above.
Object types don't contain any values themselves, they expose subfields that can be selected by the API consumer.
Their purpose is to group values in a single structure (in PureScript often referred to as Sum Types).
An object type without any fields is not of much use and in fact it is forbidden in GraphQL.
An object type must at least have one field.

We can define fields on object types by using the `withField` function.

```purescript
todoType :: GraphQL.ObjectType Effect Todo
todoType =
  GraphQL.withField
    (GraphQL.objectType "Todo")
    (GraphQL.field "id" GraphQL.id)

-- or in infix notation
todoType :: GraphQL.ObjectType Effect Todo
todoType =
  GraphQL.objectType "Todo" `GraphQL.withField` GraphQL.field "id" GraphQL.id

-- or even better with the DSL operator imported from GraphQL
todoType :: GraphQL.ObjectType Effect Todo
todoType = GraphQL.objectType "Todo"
  .> GraphQL.field "id" GraphQL.id
```

We can add descriptions to objects and fields.
These descriptions are available via _introspection_ a way to query an API about its structure and data offerings.

```purescript
todoType :: GraphQL.ObjectType Effect Todo
todoType = GraphQL.objectType "Todo"
  :> "A todo holds information about a task and its completion status"
  .> GraphQL.field "id" GraphQL.id
```
