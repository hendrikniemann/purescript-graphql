# Building the HTTP server

GraphQL itself does not specify the transportation layer of the client server communication.
In our case we will simply go with HTTP - the most common protocol to transport GraphQL queries and responses.
The `purescript-httpure` package will help us build a web server that can respond to HTTP post requests that contain a GraphQL query.
PureScript GraphQL does not support subscriptions yet, otherwise we would also need to provide a technology that allows the server to _push_ data to the client (e.g. Web Sockets).

> **Note:**
> We will create a HTTP server that roughly implements the [GraphQL over HTTP specification](https://github.com/graphql/graphql-over-http/blob/master/spec/GraphQLOverHTTP.md).
> To keep things simple we will leave out a few details, e.g. checking for the `Content-Type` header if the format is really JSON and responding to `GET` requests.
> But none of these things should be to hard to implement, they just clutter the examples below.

## More dependencies

The `purescript-aff` package defines the `Aff` monad.
The `Aff` monad is used to describe asynchronious effects.
When writing APIs we usually deal with a lot of asynchronious IO (read and write).
For example we might want to read records from a database and then return them to the client.

## GraphQL requests over HTTP POST

As mentioned in the beginning we will be using `purescript-httpure` package to build a simple web server. For that we will again be editing our `src/Main.purs` file. If you want to go over this chapter rather quickly that is also fine since this toturial is not about building HTTP servers. Though, we will learn a lot about the parameters that can be contained in a GraphQL request and how we map them to the `graphql` function, that executes our queries.

GraphQL requests are usually transmitted over HTTP. The GraphQL language specification does not specify the protocol but HTTP is the easiest way for client-server communication when building web applications. Our GraphQL server should be able to respond to POST requests. Some GraphQL servers also accept GET requests but we will keep it simple and only support POST requests.

To create a server we will start with the basic POST example from HTTPure.

```purescript
import Effect.Console as Console
import HTTPure as HTTPure

router :: HTTPure.Request -> HTTPure.ResponseM
router { body, method: HTTPure.Post } = HTTPure.ok body
router _                              = HTTPure.notFound

main :: HTTPure.ServerM
main = HTTPure.serve 8080 router $ Console.log "Running server..."
```

The body of the request that our server accepts has to be in JSON format. Now that we have the body of the request we can try and parse it. The body _must_ contain the `query` variable and additionally may also contain `variables` and `operationName`. We will write a function `decodeParams` that takes a JSON value and returns a record containing the important GraphQL request parameters.

```purescript
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Argonaut (Json, decodeJson, (.:), (.:?))

type GraphQLParams =
  { query :: String
  , variables :: Maybe (Map String Json)
  , operationName :: Maybe String
  }

decodeParams :: Json -> Either String GraphQLParams
decodeParams json = do
  obj <- decodeJson json
  query <- obj .: "query"
  variables <- map fromFoldableWithIndex <$>
    ((obj .:? "variables") :: Either String (Maybe (FO.Object Json)))
  operationName <- obj .:? "operationName"
  pure $ { query, variables, operationName }
```

Now it is time to change the router implementation to answer GraphQL requests that are sent over `POST`.
Using pattern matching we can easily select requests that go to `POST /graphql` and only answer those.

```purescript
import Data.Either (Either(..))
import Data.Argonaut (stringify, jsonParser)

router :: HTTPure.Request -> HTTPure.ResponseM
router { body, method: HTTPure.Post, path: [ "graphql" ] } =
  case jsonParser body >>= decodeParams of
    Left error -> HTTPure.badRequest error
    Right { query, variables, operationName } -> do
      result <- graphql schema query unit context variables operationName
      HTTPure.ok $ stringify result
router _ = HTTPure.notFound
```

This is it. Start the server with `pulp run` and connect to it using a desktop installation of [GraphQL Playground](https://github.com/prisma/graphql-playground) on `http://localhost:8080/graphql`. You should now be able to run the hello world query inside of playground.
