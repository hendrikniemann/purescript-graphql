# Building the HTTP server

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

The body of the request that our server accepts has to be in JSON format. Now that we have the body of the request we can try and parse it. The body must contain the `query` variable and additionally may also contain `variables` and `operationName`. We will write a function that takes a string and returns a record of the following shape:

```purescript
import Data.Argonaut ((.?), (.??))
import Data.Argonaut as A

type GraphQLParams =
  { query :: String
  , variables :: Maybe A.Json
  , operationName :: Maybe String
  }

instance decodeJsonGraphQLParams :: DecodeJson GraphQLParams where
  decodeJson json = do
    obj <- decodeJson json
    query <- obj .? "query"
    variables <- obj .?? "variables"
    operationName <- obj .?? "operationName"
    pure { id, title, categories, content }
```

After we received the parsed JSON we can try to convert it to an object. For each step that can go wrong here we want to simply return `Left String` with an error message. Remember, the query needs to contain the `query` parameter and we can fail if it does not contain it or if it is not a string
