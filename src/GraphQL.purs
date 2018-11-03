module GraphQL
       ( graphql
       ) where

import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn6, runFn6)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import GraphQL.Type (Schema)
import Prelude (($))

graphql :: ∀ a b.
  Schema a b -> String -> b -> a -> Maybe Json -> Maybe String -> Aff Json
graphql schema query root context variables operationName =
    fromEffectFnAff $ runFn6 _graphql schema query root context nVariables nOperation
      where
        nVariables = toNullable variables
        nOperation = toNullable operationName

foreign import _graphql :: ∀ a b.
  Fn6 (Schema a b) String b a (Nullable Json) (Nullable String) (EffectFnAff Json)
