module GraphQL.Language (parse) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Aff (Error, error)
import GraphQL.Language.AST (DocumentNode)
import GraphQL.Language.Parser (document)
import Text.Parsing.StringParser (runParser)

-- | Parse a query string and return a parsed GraphQL document or an error.
parse :: String -> Either Error DocumentNode
parse = runParser document >>> lmap (show >>> error)
