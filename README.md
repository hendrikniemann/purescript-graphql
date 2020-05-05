# Purescript GraphQL

PureScript GraphQL is a [GraphQL](https://graphql.org) implementation written in PureScript. It features a powerful DSL that will make building GraphQL servers as typesafe as possible.

> ### 🚧 Full rewrite happening 🚧
>
> This is a complete rewrite of PureScript GraphQL. If you are looking for version 1 of PureScript GraphQL make sure to check out the [1.0.1 tag](https://github.com/hendrikniemann/purescript-graphql/tree/v1.0.1).
>
> Version 2 of PureScript GraphQL will be written in PureScript and no longer depend on the JavaScript implementation under the hood. This is to support user defined execution through deriving from the classic resolver model where child resolvers are only called after the parent value has arrived.

[![License](https://img.shields.io/github/license/hendrikniemann/purescript-graphql.svg)](https://github.com/hendrikniemann/purescript-graphql/blob/master/LICENSE)

## Future API

This is a sneak peak of the future PureScript GraphQL API. If you want to give feedback please open an issue.

```purescript
module Main where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Map as Map
import Effect (Effect)
import Effect.Console as Console
import GraphQL (graphql)
import GraphQL.Type ((!>), (.>), (:>))
import GraphQL.Type as GraphQL
import GraphQL.Type.Scalar as Scalar

main :: Effect Unit
main = do
  result <- graphql schema "{ hello }" Map.empty Nothing (pure unit)
  Console.log $ stringify result -- {"data":{"hello":"world"}}

schema :: GraphQL.Schema Effect Unit
schema = GraphQL.Schema { query: queryType, mutation: Nothing }

queryType :: GraphQL.ObjectType Effect Unit
queryType =
  GraphQL.objectType "Query"
    .> "The root query type."
    :> GraphQL.field "hello" Scalar.string
      .> "A simple field that always returns \"world\"."
      !> (\_ _ -> pure "world")
```

## PureScript GraphQL v2 missing features

This is a list of features still required to be implemented before v2 can have a first alpha release:

- [ ] Spec compliant error handling
  - [x] Errors contain path
  - [ ] Errors contain location
  - [ ] Errors propagate up to nearest nullable field
- [ ] Interface Types
- [ ] Union Types
- [ ] Input object types
- [x] Optional arguments
- [x] Fragments
- [ ] Fragment type conditions
- [ ] Type checks for argument definitions
      (`($arg: String!)` can only be used in places where `String` is expected)
- [x] Introspection
- [x] \_\_typename meta field on object types
