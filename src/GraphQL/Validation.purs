module GraphQL.Validation (validate) where

import Data.Function.Uncurried (Fn2, runFn2)
import Effect.Aff (Error)
import GraphQL.Document (Document)
import GraphQL.Type (Schema)

-- | Validate the AST of a query against a given schema. Returns an array of
-- | errors. The array is empty if all validation pass. Currently only supports
-- | the default validations from `graphql-js`.
validate :: ∀ a b. Schema a b -> Document -> Array Error
validate = runFn2 _validate

foreign import _validate :: ∀ a b. Fn2 (Schema a b) Document (Array Error)
