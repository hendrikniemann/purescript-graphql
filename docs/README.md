# Introduction

## About PureScript GraphQL

This is a tutorial on Purescript GraphQL (`purescript-graphql`), a wrapper for PureScript around the JavaScript GraphQL reference implementation [GraphQL.js](https://github.com/graphql/graphql-js) (`graphql-js`) initially developed by Facebook. The JavaScript ecosystem is definitely the most prominent one around GraphQL (even though historically neither the first client nor the first server implementation was written in JavaScript). With PureScript we can make use of the JavaScript ecosytem while at the same time enjoying typesafety and all the programming patterns we love (and hate) from pure functional programming.

To make the use of PureScript GraphQL more idomatic it does a few type conversions. While GraphQL.js works with JavaScript's Promises, the PureScript ecosystem favours the Aff monad. Furthermore nullable values are converted to `Maybe`s. These conversions come at a price but should still be worth it compared to a full PureScript GraphQL implementation since the whole parsing and execution engine is written in JavaScript and should be very fast.

## Prerequisites

This tutorial assumes that you are already familiar with some PureScript and the basics of GraphQL. You can learn the basics of GraphQL on the offical [GraphQL website](http://graphql.org). If you are already very familiar with GraphQL and the JavaScript implementation this tutorial can also be a nice comparison between the languages.

For this tutorial we will be using PureScript 0.12, Pulp and Bower. You can find out how to install them on the [PureScript website](http://www.purescript.org/). All three of them need [Node.js](http://nodejs.org) and we will also make use of NPM, that comes with Node.js, to install some JavaScript dependencies.

To try out and execute our GraphQL queries we will need a GraphQL client. Theoretically you can use CURL or Postman but I recommend the [GraphQL Playground](https://github.com/prisma/graphql-playground).

## Why GraphQL

GraphQL is a statically typed query language and since this is a tutorial for PureScript we don't have to discuss the benefits of strong type systems. GraphQL lets us create powerful APIs that are fun to build and fun to use. GraphQL APIs are easy to extend because if we follow a few rules we can make changes without breaking our clients.

## Why PureScript

PureScript is a powerful statically typed functional programming language. The static type system of PureScript pairs well with GraphQL since we can enforce that the application code actually returns the data that the API level needs at compile time. In comparison, GraphQL.js also checks the values of all leaf types at runtime but for many bugs this might already be to late. GraphQL.js also comes with Flow types. Unfortunately the Flow type system is not strong enough to express the complex raltionships between the GraphQL types. This leaves the resolvers untyped - the main source of errors since implementing resolvers is the bread and butter of GraphQL APIs. While there are [effords in the community](https://github.com/prisma/graphqlgen) to fix this issue with code generation, PureScript GraphQL makes use of PureScripts unique type system (especially row types) to ensure typesafety across the whole codebase. In PureScript GraphQL types of resolvers are automatically infered from the fields return type and arguments.

While modern async JavaScript is certainly way more fun to write than the infamous callback hell of good old ES5, there are still many patterns of functional programming that are helpful when writing the resolvers.

## How to make the most of this tutorial

To make the most out of this tutorial I would highly recommend to type the code into the editor instead of just copy-pasting. This tutorial attempts to provide small enough bits to make you follow along. You can find a running version of this tutorial's codebase in the [PureScript GraphQL example repository](https://github.com/hendrikniemann/purescript-graphql-example).

When you get stuck set yourself a time to try and resolve the problem on your own. If your are not able to solve the problem without help I would recommend the [functional programming Slack community](https://fpchat-invite.herokuapp.com/). The PureScript community is very active and beginner friendly. The _purescript-beginner_ slack channel is the best place to get quick help.
