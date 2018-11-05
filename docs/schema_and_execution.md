# Schema and Execution

## The GraphQL Schema

Now it is finally time to learn about GraphQL. The schema defines the _shape_ of your GraphQL API. The schema acts as the root node of the API definition graph. The schema can have two root operation object types (in fact there are three operation types but PureScript GraphQL currently does not support the subscription operation). Each type defines the fields that are available for each operation. We will learn later what exactly that means. To define the schema of our API we create a new `Schema` module in PureScript by creating a new file in the `src` folder called `Schema.purs`.

Next we want to import the `GraphQL.Type` module. We will use a qualified import since the module provides us with most of the tools we need to create a GraphQL API and we would otherwise quickly get lost.

```purescript
module Schema (schema) where

import Prelude
import GraphQL.Type as GraphQL
```

The `GraphQL.Type` module exports the function `schema` that allows us to create a GraphQL schema. Let's have a look at its type definition:

```purescript
schema :: forall a ctx.
  ObjectType ctx (Maybe a) -> Maybe (ObjectType ctx (Maybe a)) -> Schema ctx a
```

The Schema type takes two type parameters. The first one is the context type, the second one is the root value type. We will learn later what these types actually are but for now I just want to put emphasis on the fact that both types reoccur in the two arguments of the function: The schema function takes two object types (one of them optional) and both object types must have the same context and root value type as the schema that is created.

As mentioned above a schema can take two root object types for the operations. The query operation is always needed. The mutation operation is optional, therefore we can supply `Nothing` in place of the mutation operation.

The following code will create the simlest schema possible: The context as well as root value type are both of type `Unit`. It does not define a mutation type but it references a `queryType` that we will implement in the next step.

```purescript
import Data.Maybe (Maybe(..))
import Schema.Query (queryType)

schema :: GraphQL.Schema Unit Unit
schema = GraphQL.schema queryType Nothing
```

## Our first GraphQL query

To get to taste some of that sweet GraphQL as soon as possible we will try to create a schema that can answer the query `{ helloWorld }` and will then move on to our more practical blog API implementation later. To implement the `queryType` lets create a module `Schema.Query` in `src/Schema/Query.purs`.

The schema that we want to create can be expressed in the schema definition language:

```graphql
type Query {
  helloWorld: String!
}
```

While a lot of JavaScript projects have turned to defining the schema entirely in the schema definition language we need our schema to be written in PureScript so that we can statically type check the schema and the code. Our `Query` type looks like this. If you have been using `graphql-js` in the past this will look very familiar.

```purescript
module Schema.Query (queryType) where

import Prelude
import Data.Maybe (Maybe(..))
import GraphQL.Type as GraphQL

queryType :: GraphQL.ObjectType Unit (Maybe Unit)
queryType =
  GraphQL.objectType
    "Query"
    (Just "This is the root query operation type")
    { helloWorld:
        GraphQL.field'
          (GraphQL.nonNull GraphQL.string)
          (Just "A simple Hello World query")
          \_ _ -> pure "Hello World!"
    }
```

The `queryType` is a `GraphQL.ObjectType` and has the name `"Query"`. The next argument is the description of the type. In GraphQL every type can have an optional description. It is generally considered good practice to write descriptions to all your types and fields. We also added one single field to the object type named `helloWorld`. The field has the return value of non-nullable string and always returns `"Hello World!"`.

To execute our function we return to the initialised `Main` module and use the `graphql` function directly from the `GraphQL` module. The module structure and interface of PureScript GraphQL is designed to closely mirror the `graphql-js` library. Don't worry if you don't understand all the arguments of the `graphql` function, we will touch this function only twice in this tutorial. After that you don't have to know much about this function in your daily work.

```purescript
module Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log, error)
import GraphQL (graphql)
import Schema (schema)

main :: Effect Unit
main = runAff_ logResult $
  graphql schema "{ helloWorld }" unit unit Nothing Nothing
    where
      logResult (Left error)   = error $ show error
      logResult (Right result) = log $ stringify result
```

The `graphql` function takes a lot of arguments. All these arguments are required to execute a query.

1. The schema contains the API structure definition and the resolvers (functions that get executed when a specific field is requested).
2. The query is a GraphQL query string.
3. The root value is an initial value that is supplied to every operation object type as root field. We don't care about this value to much and will simply supply `unit`.
4. The same applies to the context of the request. The context value is supplied to **every** resolver but we will for now simply use `unit`.
5. Optionally we can supply a JSON value for variables used in the query. Our query does not contain any variables so we will not supply anything here.
6. Lastly we can also specify an operation name. This way we can define multiple operations in the query and define which query we want to run. In this case we have just defined a single (unnamed) operation and we don't need to specify the operation name.

In the next chapter we will build a HTTP server that maps POST request to the graphql function. Afterwards we can use a GraphQL editor to execute GraphQL queries and supply all arguments from a nice user interface.
