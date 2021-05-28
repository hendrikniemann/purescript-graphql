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
todoType :: GQL.ObjectType Context Todo
todoType = GQL.objectType "Todo"
```

In the type signature, we define the context in which our GraphQL operations are executed.
This can be any monad with a `MonadError Error` instance, like `Aff`, `Effect`, `Either Error`, etc.
Obviously, if we want to run side effects, we need an appropriate monad.
I recommend to create a fitting monad transformer stack and exporting the type to your object types.
For now we can use `Effect`, which allows us to run simple effects like logging.

The second type

```purescript
todoType :: GQL.ObjectType Effect Todo
todoType = GQL.objectType "Todo"
```
