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

## Enum types

Sometimes we have a finite set of values that we want to represent. While we could theoretically build a Scalar that only accepts these values, GraphQL offers enumerations for this particular use case. This allows us to check if a value is allowed at a certain position at the type level. Creating your own enum type is easy in PureScript GraphQL. While in JavaScript enums are by default mapped to strings we have to always define a specific value for the enum value definitions.

```purescript
data PostStatus = Draft | Published | Archived

postStatusType :: GraphQL.EnumType (Maybe PostStatus)
postStatusType =
  GraphQL.enumType
    "PostStatus"
    (Just "Describes the current publishing status of a post.")
    [ GraphQL.enumValue "DRAFT" (Just "This post is not published yet.") Draft
    , GraphQL.enumValue "PUBLISHED" (Just "This post is public.") Published
    , GraphQL.enumValue "ARCHIVED" (Just "This post has been archived.") Archived
    ]
```

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

Go ahead and save this type in a new module `Schema.Post`. We will need it later.

## Object Type Fields

Object types define their fields in a heterogeneous record type. Each field must have the same context and root value type. Field can have arguments. If we don't want to supply any arguments we can use the function `field'` from `GraphQL.Type`.

```purescript
id :: GraphQL.ObjectTypeField Unit Post
id =
  GraphQL.field'
    (GraphQL.nonNull GraphQL.id) -- The return type of this field
    (Just "A unique id for this blog post.") -- The description
    (\parent _ -> pure parent.id) -- The resolver
```

It takes a closer look to unravel the type dependencies that make PureScript GraphQL fully type safe.

```purescript
field' :: ∀ t a b ctx. OutputType (t b) ctx
  => t b
  -> Maybe String
  -> (a -> ctx -> Aff b)
  -> ObjectTypeField ctx a
```

`field'` takes the following arguments:

1. A return type that is an output type in the context of the field's context
1. A descripion
1. A resolver function that takes the root value and the context of the request and returns an asyncronious effect that resolves to the return type's root value type

This makes a few szenarios impossible that are possible with the GraphQL.js and Flow types. First we cannot write resolvers that take a different parent value than the other resolvers. All resolvers need to have the same type for this value. The same holds for the context type. All fields use the same context and we can only compose types that have the same context type in a single schema. Also resolvers must return a value of the type that the field's result type accepts as root value. This way the whole execution flow is statically type checked.

We can provoke a type error just to show how this would look like: If we change the type of the resolver to `Post -> Unit -> Aff Int` by returning an integer we will get a type error - _Could not match type Int with type String_. Not very exciting when you are used to PureScript but this has spawned [whole code generation projects](https://github.com/prisma/graphqlgen) for TypeScript. The resolver type is even fully infered when you want to supply field arguments.

This takes us to our next topic. For our blog we want to be able to receive a single blog post by its unique ID. The ID is supplied as a field argument in the GraphQL query.

```graphql
query getPost {
  post(id: "daa026e0-4852-415f-bbc2-9885f81a76a3") {
    id
    title
    content
  }
}
```

To create this type of GraphQL interface on the server side we modify the `helloWorld` query field to use the `field` function instead. This function expects an additional argument. For arguments we can supply a record of GraphQL arguments. After declaring the argument we are allowed to access the argument in the resolver. The arguments are - again - fully infered and we don't have to write types ourselves to get error messages. Instead a type class contraint will make sure that all the types match within the arguments of the `field` function.

```purescript
{ post:
    GraphQL.field
      postType
      (Just "Receive a single post from the database.")
      { id:
          GraphQL.argument
            (GraphQL.nonNull GraphQL.id)
            (Just "A unique id that belongs to a post in the database.")
      }
      \_ { id } _ ->
        pure (Just { id, title: "Hello World", content: "My first post!" })
}
```

Safe the query module and restart the server. You should now be able to execute the above and receive a post with the supplied ID. In the next chapter we will create an in memory database and adopt the implementation so that it reads from a real store.

## Input object types

Scalar types and enum types can be used as output types (to be returned from a query) as well as as input types (as field arguments inside of the query). Object types and their field resolvers on the other hand are designed to be executed. To also allow for complex inputs in record form GraphQL offers input types. Input object types are very similar to object types, they are just lacking the resolvers.

```purescript
postDraftType :: GraphQL.InputObjectType (Maybe PostDraft)
postDraftType =
  GraphQL.inputObjectType
    "PostDraft"
    (Just "A client side version of a post.")
    { title:
        GraphQL.inputField
          (GraphQL.nonNull GraphQL.string)
          (Just "The title for this post.")
    , content:
        GraphQL.inputField
          (GraphQL.nonNull GraphQL.string)
          (Just "The context for this post.")
    }
```
