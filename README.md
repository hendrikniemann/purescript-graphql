# Purescript GraphQL

PureScript GraphQL is a wrapper around [GraphQL.js](https://github.com/graphql/graphql-js). It contains powerful, typed bindings that will make building GraphQL servers as typesafe as possible. Furthermore it automatically translates `Aff`s into promises and `Maybe`s into `null` values where they would be expected by the underlying JavaScript implementation.

[![Latest release](http://img.shields.io/github/release/hendrikniemann/purescript-graphql.svg)](https://github.com/hendrikniemann/purescript-graphql/releases)
[![License](https://img.shields.io/github/license/hendrikniemann/purescript-graphql.svg)](https://github.com/hendrikniemann/purescript-graphql/blob/master/LICENSE)

## Installation

```bash
bower install purescript-graphql
npm install graphql-js
```

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-graphql).

## Getting started

This is the minimal code that is needed to execute a GraphQL query. If you want to get started with an HTTP server check out the [example repository](https://github.com/hendrikniemann/purescript-graphql-example) or follow along [the tutorial](https://hendrikniemann.github.io/purescript-graphql/#/).

```purescript
module Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console as Console
import GraphQL (graphql)
import GraphQL.Type as GraphQL

main :: Effect Unit
main = runAff_ (either (show >>> Console.error) (stringify >>> Console.log)) $
  graphql schema "{ hello }" unit unit Nothing Nothing

schema :: GraphQL.Schema Unit Unit
schema = GraphQL.schema queryType Nothing

queryType :: GraphQL.ObjectType Unit (Maybe Unit)
queryType =
  GraphQL.objectType
    "Query"
    (Just "The main query type")
    { hello:
        GraphQL.field'
          (GraphQL.nonNull GraphQL.string)
          (Just "A simple field that always returns \"world\".")
          \_ _ -> pure "world"
    }
```
