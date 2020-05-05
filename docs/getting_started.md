# Getting Started

The goal of this chapter is to setup a simple PureScript project that defines a simple GraphQL schema.
The GraphQL schema will contain exactly one query field and allows us to execute a _Hello World_ query: `{ hello }`.

## Initialising the project

We will begin in an empty folder and initialise a PureScript project using spago.
This will create a project skelleton and install the prelude package for us.

```
spago init
```

Let's verify that your installation works by running the initialised project using spago.
In the future this command will be the single command that we use to start our HTTP server but for now it should just print a string to the console and terminate.

```
spago run
```

## Installing PureScript dependencies

Next, we will install some dependencies that we will be using in the next step of this tutorial.
To create our GraphQL API we will of course use the one and only `graphql` package.
The package contains all modules that we need to decribe a GraphQL Schema and execute queries.

GraphQL query results are encoded in JSON. We will need the packages `argonaut-core` and `argonaut-codecs` to deal with the result of the GraphQL execution.

Last but not least, we also add some generally useful libraries: `maybe`, `either` and `ordered-collections`

> **Note:**
> I have been asked why I am not using `purescript-simple-json`.
> To build the result, PureScript GraphQL makes heavy use of the combinators of Argonaut.
> Argonaut now offers a superset of the operations in simple-json with the provided encoding and decoding type classes.

!> Currently `purescript-graphql` is not part of the official package set and needs to be inserted manually into the packages.
This step won't be neccessary in the near future.
Adopt the `additions` variable in the `packages.dhall` file to install from the master branch:

```dhall
let additions =
      { graphql =
        { dependencies =
          [ "argonaut-codecs"
          , "argonaut-core"
          , "console"
          , "control"
          , "effect"
          , "enums"
          , "foldable-traversable"
          , "nullable"
          , "numbers"
          , "prelude"
          , "psci-support"
          , "record"
          , "spec"
          , "string-parsers"
          ]
        , repo = "https://github.com/hendrikniemann/purescript-graphql.git"
        , version = "master"
        }
      }
```

To install all the purescript dependencies you can simply go ahead and run this command in your project directory.

```
spago install graphql argonaut-core argonaut-codecs maybe either ordered-collections
```

## Building the first schema

Now we can start editing the `src/Main.purs` file in the project.
We will start with some imports.
The `GraphQL.Type` module contains functions and operators to construct GraphQL schemas.
I like to import the module under a qualifier since the exposed functions have quite generic names.
For convenience we also import two operators `(:>)` and `(!>)`.
They are used to build object types.

```purescript
import Data.Maybe (Maybe(..))
import GraphQL.Type ((:>), (!>))
import GraphQL.Type as GQL
import GraphQL.Type.Scalar as Scalar
```

Now, let's construct a simple schema:

```purescript
schema :: GQL.Schema Effect Unit
schema = GQL.Schema { query: ?query, mutation: Nothing }
```

`Schema` takes two types:

- The context monad that we use to run our computations.
  I have simply chosen Effect because we already have it in scope and we want to eventually use the schema in the `main` function.
- The root type of the schema.
  A schema accepts a root value that is passed into all root resolvers as a parent value.
  While there are use cases for this, I seldomly use this functionality.
  Therefore we can just use the `Unit` type and indicate that we don't want to pass anything into these resolvers for now.

Now you should see a type error, telling us about the type of the typed hole:

```
  Hole 'query' has the inferred type

    ObjectType Effect Unit
```

Let's build an object type that fits into the typed hole!

```purescript
queryType :: GQL.ObjectType Effect Unit
queryType = GQL.objectType "Query"
```

Great, we now have a simple object type without fields.
Technically this is illegal in GraphQL.
We should quickly add a field to the object type.
As explained in the introduction to this chapter we want to execute a single query: `{ hello }`.
The result of the query should always be `{"data":{"hello":"world"}}`.
As we are returning a string from the _hello_ field, we chose the `string` scalar.
`GraphQL.Type.Scalar` contains all the standards scalars that the [GraphQL specification](http://spec.graphql.org/June2018/#sec-Scalars) defines.

```purescript
helloField :: GQL.Field Effect String () ()
helloField = GQL.field "hello" Scalar.string
```

If we would add the field to the query type, we would encounter a type error because our field expects a string as the root value.
It is not easy to to make out which type above is the root value type but below we will see that we will almost never deal with these types explicitly again.
A field without a resolver just passes through the parent value.
Let's add a resolver that takes a unit parent value and returns the string "world" inside of the effect monad.

```purescript
helloField :: GQL.Field Effect Unit () ()
helloField =
  GQL.withResolver
    (GQL.field "hello" Scalar.string)
    (\args parent -> pure "world")
```

...and add the field to the query type...

```purescript
queryType :: GQL.ObjectType Effect Unit
queryType = GQL.withField (GQL.objectType "Query") helloField
```

Now, admittedly it is not the prettiest code to look at.
GraphQL APIs often consist of hundreds of types an thousands of fields.
Using the operators `:>` for `withField` and `!>` for `withResolver` we can shorten our code drastically and make inlining of the field more readable.

```purescript
schema :: GQL.Schema Effect Unit
schema = GQL.Schema { query: queryType, mutation: Nothing }

queryType :: GQL.ObjectType Effect Unit
queryType = GQL.objectType "Query"
  :> GQL.field "hello" Scalar.string
    !> (\args parent -> pure "world")
```

## Executing the first query

Now we want to make use of the `graphql` function to execute a query on the schema.
Import the function from `GraphQL` as well as the qualified `Map` and `stringify` to turn our result into a string:

```purescript
import Data.Argonaut.Core (stringify)
import Data.Map as Map
import GraphQL (graphql)
```

We can now execute GraphQL queries using this function!

```purescript
graphql schema "{ hello }" Map.empty Nothing (pure unit)
```

The `graphql` function expects a hell lot of parameters.
Parameter one and two should be familiar.
They are the schema that we have just built together and the query string that will be executed.
The last parameter `(pure unit)` is the root value that we have discussed earlier.
We will ignore the other two parameters for now and integrate the call into the `main` function.

```purescript
main :: Effect Unit
main = do
  result <- graphql schema "{ hello }" Map.empty Nothing (pure unit)
  log $ stringify result
```

We can run our program with `spago run` and should see the result in the console!

In the next chapter we will hook up the query execution with an HTTP server to make GraphQL requests over the network.
