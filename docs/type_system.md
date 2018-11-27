# GraphQL Type System

## Scalar Types

Scalar types are types that consist of a single value. Therefore scalar types usually translate to a primitive value in the JSON result. PureScript GraphQL comes with all the standart scalar types defined in the GraphQL language specification: `ID`, `String`, `Int`, `Float` and `Boolean`. They are available in the `GraphQL.Types` package in lowercase e.g. `id` or `boolean`. Scalar types can be used as input type as well as output type. Internally the scalar type contains methods to transform the values from and into their JSON representation.

Currently it is not possible to create your own custom scalars with PureScript GraphQL but this functionality will follow soon.

In our `helloWorld` query we have already made use of the string scalar type as the return type of the field:

```purescript
GraphQL.field'
  (GraphQL.nonNull GraphQL.string)
  (Just "A simple Hello World query")
  \_ _ -> pure "Hello World!"
```

In GraphQL all types are nullable by default. This is why the type of the `string` scalar function is `Scalar (Maybe String)`. To receive a non-nullable type we have to call the `nonNull` function with the type. `nonNull` supports not only scalar types but any GraphQL type.

## Object Types

Object types are GraphQL's equivalent to records. Objects are key-value pairs that map directly to JSON objects. This means that only strings are allowed as keys. Objects don't have to be heterogeneous but must define a type for each paivaluer - just like records in PureScript. Since everything in GraphQL can be annotated with a description we have the ability to supply an optional description to the type as well as the fields. Adding and maintaining a description is considered best practise.

Let's look at a practical example. In this tutorial we want to build an API to serve a small blogging website. The `Post` type will be the center of the API. We will be able to read, create, update and delete blog posts. To keep it simple it will for now contain an unique ID to identify the post accross requests, a title and some content. I chose to go with a type alias here but you can of cause also use a newtype. In the end we are just working with pure functions.

```purescript
type Post = { id :: String, title :: String, content :: String }
```

Now we can create a matching object type for our GraphQL schema. We use the `objectType` function from `GraphQL.Type` and supply a name, a description and a field map. This results in a `ObjectType` that we can use anywhere in our schema as a return value. `ObjectType` takes two type parameters: The first one - _Context_ - describes the context type of the request. The context holds the state of the request. We will have a deeper look at the request context in the next chapter. Since the context is not used by any of our resolvers the post type will work with any context and we could have chosen to express this `forall a. GraphQL.ObjectType a (Maybe Post)`. For simplicity we will use the unit type and later replace it with the context type we use across the whole API. The second parameter is the parent value that this GraphQL type expects. In our case we expect a field with return type `postType` to supply us with `Maybe Post`.

```purescript
postType :: GraphQL.ObjectType Unit (Maybe Post)
postType =
  GraphQL.objectType
    "Post"
    (Just "A blog post that is persisted in the database.")
    { id:
        GraphQL.field'
          (GraphQL.nonNull GraphQL.id)
          (Just "A unique id for this blog post.")
          (\parent _ -> pure parent.id)
    , title:
        GraphQL.field'
          (GraphQL.nonNull GraphQL.string)
          (Just "The title of this blog post.")
          (\parent _ -> pure parent.title)
    , content:
        GraphQL.field'
          (GraphQL.nonNull GraphQL.string)
          (Just "The title of this blog post.")
          (\parent _ -> pure parent.content)
    }
```
