module GraphQL.Language (parse) where

-- | Parse a query string and return a parsed GraphQL document or an error.
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import GraphQL.Document (Document)
import Effect.Aff (Error)

-- | Parse a query string and return a parsed GraphQL document or an error.
parse :: String -> Either Error Document
parse = runFn3 _parse Left Right

foreign import _parse :: ∀ a b. Fn3
  (a -> Either a b)
  (b -> Either a b)
  String
  (Either Error Document)