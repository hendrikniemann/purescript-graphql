module GraphQL.Type.Schema (Schema(..)) where

import Data.Maybe (Maybe)
import GraphQL.Type.ObjectType (ObjectType)


-- | The schema contains the central entry points for GraphQL queries.
newtype Schema m a = Schema { query :: ObjectType m a, mutation :: Maybe (ObjectType m a) }
