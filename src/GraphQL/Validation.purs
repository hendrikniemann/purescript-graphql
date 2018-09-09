module GraphQL.Validation (validate) where

import Data.Function.Uncurried (Fn2, runFn2)
import Effect.Exception (Error)
import GraphQL.Document (Document)
import GraphQL.Type (Schema)

-- | Validate the AST of a query against a given schema. Returns an array of
-- | errors. The array is empty if all validation pass. Currently only supports
-- | the default validations from `graphql-js`.
validate :: ∀ a. Schema a -> Document -> Array Error
validate = runFn2 _validate

foreign import _validate :: ∀ a. Fn2 (Schema a) Document (Array Error)
