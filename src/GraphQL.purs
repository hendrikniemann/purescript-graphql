module GraphQL
       ( graphql
       ) where

import Prelude (($))

import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn5, runFn5)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import GraphQL.Type (Schema)

graphql :: ∀ a. Schema a -> String -> a -> Maybe Json -> Maybe String -> Aff Json
graphql schema query root variables operationName =
    fromEffectFnAff $ runFn5 _graphql schema query root nVariables nOperation
      where
        nVariables = toNullable variables
        nOperation = toNullable operationName

foreign import _graphql :: ∀ a.
  Fn5 (Schema a) String a (Nullable Json) (Nullable String) (EffectFnAff Json)
