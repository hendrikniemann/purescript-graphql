module Data.GraphQL.Execution (execute) where

import Prelude (($))

import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.GraphQL.Document (Document)
import Data.GraphQL.Type (Schema)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

-- | Asyncroniously executes a query given the GraphQL document and a schema.
execute :: ∀ a. Schema a -> Document -> a -> Aff Json
execute scm document root = fromEffectFnAff $ runFn3 _execute scm document root

foreign import _execute :: ∀ a. Fn3 (Schema a) Document a (EffectFnAff Json)
