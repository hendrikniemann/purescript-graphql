# Union Types and Interfaces

## Sum types in GraphQL

Sometimes in our API, we want to return a values of different types.
Imagine the following example:
In a Twitter-like social media app, we have a stream of posts.
Posts can contain some media, either a link/url, an image or a video.

In PureScript we could model it like this:

```purescript
newtype Post = Post { id :: Int, body :: String, media :: Maybe Media }

data Media
  = Link { url :: String, title :: String }
  | Image { url :: String, altText :: Maybe String }
  | Video { url :: String, length :: Int }
```

In GraphQL, we can use the union type to create a sum type of object types.

```graphql
type Post {
  id: ID!
  body: String!
  media: Media
}

type Link {
  url: String!
  title: String!
}

type Image {
  url: String!
  altText: String
}

type Video {
  url: String!
  length: Int!
}

union Media = Link | Image | Video
```

An alternative way to create sum types are `Variant`s.
Variants use row types instead and let us create more reusable functions.
Variants also allow for type level programming.

```purescript
type MediaVariant = Variant
  ( link :: { url :: String, title :: String }
  , image :: { url :: String, altText :: Maybe String }
  , video :: { url :: String, length :: Int }
  )
```

## Using union types in PureScript GraphQL

Variant types are not that easy to use, because we have to use `Proxy` to handle type level strings in our code.
But they allow PureScript GraphQL to generate a Variant type, when we supply a record of object types for the union type.
To build a union type in PureScript GraphQL, we use the `union` function from `GraphQL.Type` (here imported as `GQL`):

```purescript
-- This assumes you already built the following object types:
-- linkType :: GQL.ObjectType Context { url :: String, title :: String }
-- imageType :: GQL.ObjectType Context  { url :: String, altText :: Maybe String }
-- videoType :: GQL.ObjectType Context  { url :: String, length :: Int }

mediaType :: GQL.UnionType Context MediaVariant
mediaType = GQL.union "Media" { link: linkType, image: imageType, video: videoType }
```

Notice how the record type used in the definition is very similar to the variant type.

```purescript
record :: Record
  ( link :: GQL.ObjectType Context { url :: String, title :: String }
  , image :: GQL.ObjectType Context { url :: String, altText :: Maybe String }
  , video :: GQL.ObjectType Context { url :: String, length :: Int }
  )
record = { link: linkType, image: imageType, video: videoType }

type MediaVariant = Variant
  ( link :: { url :: String, title :: String }
  , image :: { url :: String, altText :: Maybe String }
  , video :: { url :: String, length :: Int }
  )
```
