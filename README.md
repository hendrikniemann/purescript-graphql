# Purescript GraphQL

[![License](https://img.shields.io/github/license/hendrikniemann/purescript-graphql.svg)](https://github.com/hendrikniemann/purescript-graphql/blob/master/LICENSE)

PureScript GraphQL is a [GraphQL](https://graphql.org) implementation written in PureScript. It features a powerful DSL that makes building GraphQL servers as typesafe as possible.

> ### 🚧 Full rewrite happening 🚧
>
> This is a complete rewrite of PureScript GraphQL. If you are looking for version 1 of PureScript GraphQL make sure to check out the [1.0.2 tag](https://github.com/hendrikniemann/purescript-graphql/tree/v1.0.2).

To follow the roadmap to version 2 check out the [milestone](https://github.com/hendrikniemann/purescript-graphql/milestone/1)

## Getting started

If you are new to PureScript GraphQL I highly recommend to follow the [tutorial](https://hendrikniemann.github.io/purescript-graphql/). ~~If you know what you are doing you can simply install `purescript-graphql` with spago.~~ (Not in the package set yet, will be done as soon as beta is out)

```
spago install graphql
```

## Basic example

This code creates a simple schema with one query field.
The field takes one argument `name` and returns a string.

```purescript
module Main where

import Prelude

import Data.Argonaut (stringify)
import Data.Map as Map
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console as Console
import GraphQL ((!>), (.>), (:>), (?>), graphql)
import GraphQL as GraphQL

main :: Effect Unit
main = do
  result <- graphql schema """{ hello(name: "world") }""" Map.empty Nothing unit
  Console.log $ stringify result -- {"data":{"hello":"world"}}

schema :: GraphQL.Schema Effect Unit
schema = GraphQL.Schema { query: queryType, mutation: Nothing }

queryType :: GraphQL.ObjectType Effect Unit
queryType =
  GraphQL.objectType "Query"
    .> "The root query type."
    :> GraphQL.field "hello" GraphQL.string
      ?> GraphQL.arg GraphQL.string (SProxy :: SProxy "name")
      .> "A simple field that returns a greeting."
      !> (\{ name } _ -> pure $ "Hello, " <> name <> "!")
```
