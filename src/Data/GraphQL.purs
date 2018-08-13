module Data.GraphQL
       ( graphql
       ) where

import Prelude (($))

import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.GraphQL.Type (Schema)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

graphql :: ∀ a. Schema a -> String -> a -> Aff Json
graphql schema query root = fromEffectFnAff $ runFn3 _graphql schema query root

foreign import _graphql :: ∀ a. Fn3 (Schema a) String a (EffectFnAff Json)
