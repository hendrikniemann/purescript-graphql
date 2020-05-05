# Introduction

## About PureScript GraphQL

PureScript GraphQL is a feature-lacking, production-not-ready and unblazingly slow GraphQL implementation.
Okay, let me rephrase that: PureScript is a highly experimental, end-to-end-typesafe and pure functional GraphQL implementation.
It serves mainly two purposes:

1. Explore the combination of functional programming concepts with GraphQL servers
2. Act as a motivating real life project for me to learn pure functional programming

For you on the other hand, this implementation and the following tutorial can be fun to explore if you either come from a functional programming background and you want to learn about GraphQL or if you are familiar with GraphQL in mainstream programming languages and want to get inspired by the things that are pretty much only possible in PureScript.
Here are some of my proudest features:

- Resolvers are pure functions (no surprises here)
- Resolver types are fully inferred
  - Parent type, return type and argument type are fully type inferred
  - Arguments translate to records just like in GraphQL.js
  - Use the powerful type system of PureScript - no runtime errors\*!
  - No codegen in the background like in GraphQL Nexus
- Free choice of execution context
  - Use `Either Error` for pure computations
  - Use `Effect` for effectful computations
  - Use `Aff` for asynchronous effectful computations
  - Use your own monad transformer stack for your production-grade web app
- Value level GraphQL schema to build your own abstractions
- E.g. soon available: Magically infer input and output types for your record types

_\* Maybe not quite no runtime errors as in zero runtime errors, but - you know - less runtime errors than JS._

## Prerequisites

This tutorial assumes that you are already familiar with some PureScript and the basics of GraphQL.
You can learn the basics of GraphQL on the offical [GraphQL website](http://graphql.org).
I hate to make use of the M word here but monads are in the center of effectful code in PureScript and obviously this library.
You should understand how to work with effect monads like `Effect`, `Aff` or `IO` if you come from Haskell.

For this tutorial we will be using PureScript 0.13 and Spago.
You can find out how to install them on the [PureScript getting started guide](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md).
You will also need [Node.js](http://nodejs.org) and in the later stages of this tutorial we will also make use of NPM to install some JavaScript dependencies.
NPM comes with Node.js and you don't have to install it.

To try out and execute our GraphQL queries over HTTP we will need a GraphQL client.
In theory, you can use CURL or Postman but I recommend the [GraphQL Playground](https://github.com/prisma-labs/graphql-playground).
Unlike in other GraphQL ecosystems, there is no middleware package available that serves Playground or GraphiQL.
We will have to build our own middleware and we want to keep it simple.

## Why GraphQL?

GraphQL is a statically typed query language and since this is a tutorial for PureScript we don't have to discuss the benefits of strong type systems.
GraphQL lets us create powerful APIs that are fun to build and fun to use.
GraphQL APIs are easy to extend because if we follow a few rules we can make changes without breaking our clients.

## Why PureScript?

PureScript is a powerful statically typed functional programming language.
The static type system of PureScript pairs well with GraphQL since we can enforce that the application code actually returns the data that the API level needs at compile time.
In comparison, GraphQL.js also checks the values of all leaf types at runtime but for many bugs this might already be to late.
GraphQL.js also comes with Flow and TypeScript types.
Unfortunately neither the Flow type system nor the TypeScript type system is strong enough to express the complex raltionships between types in GraphQL schemas.
This leaves the resolvers untyped.
The resovlers become a source of errors since implementing resolvers is the bread and butter of GraphQL APIs.
While there are [effords in the community](https://github.com/graphql-nexus/nexus) to fix this issue with code generation, PureScript GraphQL makes use of PureScripts unique type system (especially row types) to ensure typesafety across the whole codebase.
In PureScript GraphQL, the types of resolvers are automatically infered from the fields return type and arguments.

While modern async JavaScript is certainly way more fun to write than the infamous callback hell of common ES5, there are still many patterns of functional programming that are helpful when writing the resolvers.
Attempts of the JavaScript and TypeScript community to bring functional concepts to these languages are admireable but result in dramatic readability tradeoffs.
The simple lack of language features like generalised `do`-notation, auto-currying, a compose operator and pattern matching make the introduced features unattractive and a dedicated library to wrap GraphQL.js (similar to how PureScript v1 wraps GraphQL.js) would be needed to at least remove constant type conversion (e.g. between `null` and `Maybe`).
PureScript solves this with a compile step and this library now also allows the written PureScript code to make use of even more powerful concepts like monad transformers.

## How to make the most of this tutorial

To make the most out of this tutorial I would highly recommend to type the code into the editor instead of just copy-pasting.
This tutorial attempts to provide small enough bits to make you follow along.
You can find a running version of this tutorial's codebase in the [PureScript GraphQL example repository](https://github.com/hendrikniemann/purescript-graphql-example).

When you get stuck, set yourself a time to try and resolve the problem on your own.
If your are not able to solve the problem without help I would recommend the [functional programming Slack community](https://fpchat-invite.herokuapp.com/).
The PureScript community is very active and beginner friendly.
The _purescript-beginner_ slack channel is the best place to get quick help.
