# A functional programmers guide to GraphQL

> In this guide we will have a look at GraphQL coming from a functional programming background.
> It is very brief because I believe the best way of learning GraphQL is through building and exploring.
> Instead, this section is supposed to teach you just enough to get started using PureScript GraphQL

GraphQL is a query language for APIs that expose graph-like data structures.
A GraphQL API exposes all available data and operations in a giant graph.
The API consumer then uses GraphQL to select a subset of the data (a sub-graph).
Compared to other (graph) query languages, e.g. Cypher for Neo4j or SQL, GraphQL is a rather weak language that leaves room for custom implementation specific logic.
A GraphQL query mostly only consists of _selection sets_, _fields_ and _field arguments_.

## GraphQL schema and type system

Every GraphQL endpoint exposes exactly one schema.
The schema is a description of which queries can be executed on the GraphQL API.
GraphQL has a simple type system to describe this structure.
This type system features scalars for promitive types, object types for record structures and union types for sum types.

## GraphQL resolver model

There are many ways how a GraphQL query can be translated into a corresponding result.
PureScript GraphQL uses the well established resolver model.
In the resolver model, every field is paired with a function that describes how the provided field arguments together with the parent value form the field's value.
In PureScript GraphQL, a field can also perform some form of side effects, if the schema allows for it.
Let's look at some simple resolver examples:

```purescript
-- Root resolver loads row from database
todo :: { id :: String } -> Unit -> Aff (Todo { title :: String, isCompleted :: Boolean })
todo args parent = loadTodoById args.id

-- Field with no arguments resolves to parent field
isCompleted :: {} -> Todo { title :: String, isCompleted :: Boolean } -> Aff String
isCompleted args (Todo { isCompleted }) = pure isCompleted
```

## GraphQL over HTTP

The GraphQL specification itself deliberately leaves out specifics about the transport layer.
GraphQL queries can be sent via HTTP, websockets or any other text format imaginable.
On the web, we usually find GraphQL servers that use HTTP for queries and mutations and websockets for subscriptions.
PureScript GraphQL does not provide transport layer functionality, but we are going to build an HTTP server in one of the chapters of this documentation.
