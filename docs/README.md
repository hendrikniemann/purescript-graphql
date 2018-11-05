# Introduction

## About PureScript GraphQL

This is a tutorial on `purescript-graphql`, a wrapper for PureScript around the JavaScript reference implementation `graphql-js` developed by Facebook. The JavaScript ecosystem is definitely the most prominent one around GraphQL (even though historically neither the first client nor the first server implementation was written in JavaScript). With PureScript we can make use of the JavaScript ecosytem while at the same time enjoying typesafty and all the programming patterns we love (and hate) from pure functional programming.

To make the use of PureScript GraphQL more idomatic it does a few type conversions. While `graphql-js` works heavily with JavaScript's Promises, the PureScript ecosystem favours the Aff monad. Furthermore nullable values are converted to `Maybe`s. These conversions come at a price but should still be worth it compared to a full PureScript GraphQL implementation since the whole parsing and execution engine is written in JavaScript and should be very fast.

## Prerequisites

This tutorial assumes that you are already familiar with some PureScript and the basics of GraphQL. You can learn the basics of GraphQL on the offical [GraphQL website](http://graphql.org). If you are already very familiar with GraphQL and the JavaScript implementation this tutorial can also be a nice comparison between the languages.

For this tutorial we will be using PureScript 0.12, Pulp and Bower. You can find out how to install them over on the [PureScript website](http://www.purescript.org/). All three of the need [Node.js](http://nodejs.org) and we will also make use of NPM, that comes with Node.js, to install some JavaScript dependencies.

## Why GraphQL

GraphQL is a statically typed query language and this is a tutorial for PureScript so we don't have to discuss the benefits of strong type systems. GraphQL lets us create powerful APIs that are fun to build and fun to use. GraphQL APIs are easy to extend because if we follow a few rules we can make changes without breaking our clients.

## Why PureScript

PureScript is a powerful statically typed functional programming language. The static type system of PureScript pairs well with GraphQL since we can enforce that the application code actually returns the data that the API level needs at compile time. `graphql-js` also checks the values of all leaf types at runtime but for many bugs this might already be to late. `graphql-js` also comes with Flow types. Unfortunately the Flow type system is not strong enough to express the complex raltionships between the GraphQL types. This leaves the resolvers untyped - the main source of errors since implementing resolvers is the bread and butter of GraphQL APIs. We will learn more about resolvers in the next chapter.

While modern async JavaScript is certainly way more fun to write than the infamous callback hell, there are still many patterns of functional programming that are helpful when writing the resolvers.

## Before you get started

To make the most out of this tutorial I would highly recommend to type the code into the editor instead of just copy-pasting. This tutorial attempts to provide small enough bits to make you follow along. You can find a running version of this example in the [PureScript GraphQL repository](https://github.com/hendrikniemann/purescript-graphql) in the `example` subfolder.

When you get stuck set yourself a time to try and resolve the problem on your own. If your are not able to solve the problem without help I would recommend the [functional programming Slack community](https://fpchat-invite.herokuapp.com/). The PureScript community is very active and beginner friendly. The `purescript-beginner` slack channel is the best place to get quick help.
