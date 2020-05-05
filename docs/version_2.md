# What's new in Version 2 of PureScript GraphQL?

PureScript GraphQL Version 2 is here.
Version 2 is a complete rewrite and has many breaking changes.
While version 1 can be considered stable thanks to the battle tested GraphQL.js library in the background, version 2 tries to actually build a PureScript implementation for GraphQL.
With version 1, you could build GraphQL servers with PureScript.
Now you can build GraphQL servers **in** PureScript.
But what does that even mean? We are no longer bound to the interface of a mutable object oriented language and instead use functional programming concepts to achieve similar effects.

I am myself still learning these concepts one step at the time.
But I have gathered feedback from the community and came up with these interesting changes that let you dive fully into pure functional programming.

## Free Choice of Execution Context

This is the biggest change and pretty much the sole motivator for the rewrite.
The GraphQL result can now be calculated inside of the context of any monad.
Usually the execution of a schema is an effectful computation.
The user of the library is now completely free to use any effect monad of their choice as long as it provides a `MonadError Error` implementation (like e.g. `Aff` or `Effect`).
If the execution is completely pure one can even use `Either Error` to create side-effectless synchronious schemas.
But in practise our computations are not only effectful but also stateful.
Here we want to use a custom monad!
You can now build up any monad transformer stack and use it as the context of your GraphQL execution.

Furthermore, context is no longer a mutable object being passed into every resolver.
Previously PureScript GraphQL followed GraphQL.js and allowed to pass in a context object into every resolver.
But we don't need that in PureScript GraphQL.
We have `StateT`.
In the later stages of this tutorial, I use a monad stack to inject a database connection and the ID of the currently authenticated user into every resolver.

## List fields accept any Traversable

Tired of arrays?
You prefer your good old `List`?
Or sometimes you just want to directly return a `Map` from a resolver?
No problem!
PureScript GraphQL will painlessly traverse your data structure into a JSON array.
