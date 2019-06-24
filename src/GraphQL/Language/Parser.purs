module GraphQL.Language.Parser
  ( document
  , definition
  , name
  , variableDefinition
  , selectionSet
  , selection
  , value
  ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.List (List(..), reverse, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import GraphQL.Language.AST as AST
import Prim.TypeError (class Warn, Text)
import Text.Parsing.StringParser (Parser, try, fail)
import Text.Parsing.StringParser.CodePoints (regex, string)
import Text.Parsing.StringParser.CodeUnits (char, noneOf)
import Text.Parsing.StringParser.Combinators (between, many, optionMaybe)
import Unsafe.Coerce (unsafeCoerce)

undefined :: forall a. Warn (Text "Use of undefined") => a
undefined = unsafeCoerce unit

parens :: forall a. Parser a -> Parser a
parens = between (char '(') (char ')')

braces :: forall a. Parser a -> Parser a
braces = between (char '{') (char '}')

bracket :: forall a. Parser a -> Parser a
bracket = between (char '[') (char ']')

quotes :: forall a. Parser a -> Parser a
quotes = between (char '"') (char '"')

-- | Parse a valid GraphQL identifier
-- | See: https://graphql.github.io/graphql-spec/June2018/#sec-Names
name :: Parser AST.NameNode
name = AST.NameNode <$> {value: _} <$> regex "[_A-Za-z][_0-9A-Za-z]*"

-- | Parse any token that is ignored by the GraphQL parser.
-- | For performance they are all summarised in a single regex which is a bit different
-- | from the specification that knows different types of ignored tokens.
-- | See: https://graphql.github.io/graphql-spec/June2018/#sec-Source-Text.Ignored-Tokens
whiteSpace :: Parser String
whiteSpace = regex "(#[^\\n\\r]\\n\\r]|[, \\u0009\\n\\r\\uFEFF])*" -- Comments not completely correct

-- | Helper function to parse tokens wrapped in white space
token :: String -> Parser String
token s = whiteSpace *> (string s) <* whiteSpace

-- | Parse a GraphQL Document. This is the top level parser for all GraphQL source documents.
-- | See: https://graphql.github.io/graphql-spec/June2018/#sec-Language.Document
document :: Parser AST.DocumentNode
document = AST.DocumentNode <<< {definitions: _} <$> many definition

definition :: Parser AST.DefinitionNode
definition = do
  header <- optionMaybe operationHeader
  v <- variableDefinitionList <* whiteSpace <|> pure Nil
  s <- selectionSet
  let { op, n } = fromMaybe { op: AST.Query, n: Nothing } header
  pure $ AST.OperationDefinitionNode {name: n, operation: op, selectionSet: s, variableDefinitions: v }
    where
    toOperation "query" = AST.Query
    toOperation _ = AST.Mutation
    variableDefinitionList = parens $ many (whiteSpace *> variableDefinition <* whiteSpace)
    operationHeader = do
      op <- toOperation <$> (token "query" <|> token "mutation")
      n <- optionMaybe $ name <* whiteSpace
      pure { op, n }

variableDefinition :: Parser AST.VariableDefinitionNode
variableDefinition = do
  n <- char '$' *> name <* token ":"
  t <- variableType
  pure $ AST.VariableDefinitionNode { variable: n, "type": t, directives: Nil, defaultValue: Nothing}

variableType :: Parser AST.TypeNode
variableType =
  try nonNullType
    <|> namedType
    <|> listType
    where
    nonNullType = AST.NonNullTypeNode <<< { "type": _ } <$> (namedType <|> listType) <* token "!"

    namedType = AST.NamedTypeNode <<< { "type": _ } <$> simpleNamedType

    listType = AST.ListTypeNode <<< { "type": _ } <$> bracket (whiteSpace *> (defer \_ -> variableType) <* whiteSpace)

simpleNamedType :: Parser AST.SimpleNamedTypeNode
simpleNamedType = AST.SimpleNamedTypeNode <<< {name: _} <$> name

selectionSet :: Parser AST.SelectionSetNode
selectionSet =
  braces $ AST.SelectionSetNode <<< {selections: _} <<< reverse <$> many (defer \_ -> selection)

selection :: Parser AST.SelectionNode
selection =
  try (whiteSpace *> defer \_ -> field <* whiteSpace)
    <|> try (whiteSpace *> fragmentSpread <* whiteSpace)
    <|> (whiteSpace *> inlineFragment <* whiteSpace)
  where
  fragmentSpread :: Parser AST.SelectionNode
  fragmentSpread = fail "Fragment spread not yet supported"

  inlineFragment :: Parser AST.SelectionNode
  inlineFragment = fail "Inline Fragments not yet supported"

  field :: Parser AST.SelectionNode
  field = do
    a <- optionMaybe $ try $ whiteSpace *> name <* whiteSpace <* token ":"
    n <- whiteSpace *> name <* whiteSpace
    args <- (argumentList <* whiteSpace) <|> pure Nil
    s <- optionMaybe selectionSet
    pure $ AST.FieldNode {alias: a, name: n, arguments: args, directives: Nil, selectionSet: s}

argumentList :: Parser (List AST.ArgumentNode)
argumentList = parens $ many (whiteSpace *> argument <* whiteSpace)

argument :: Parser AST.ArgumentNode
argument = do
  n <- name
  _ <- whiteSpace *> char ':' <* whiteSpace
  v <- value
  pure $ AST.ArgumentNode { name: n, value: v }

value :: Parser AST.ValueNode
value =
  AST.VariableNode <<< {name: _} <$> try variableParser
    <|> AST.StringValueNode <<< {block: false, value: _} <$> try stringParser
    <|> AST.FloatValueNode <<< {value: _} <$> try floatParser
    <|> AST.IntValueNode <<< {value: _} <$> try intParser
    <|> AST.BooleanValueNode <<< {value: _} <$> try booleanParser
    <|> AST.NullValueNode <$ string "null"
    <|> AST.EnumValueNode <<< {name: _} <$> try name
    <|> AST.ListValueNode <<< {values: _} <$> try listParser
    <|> AST.ObjectValueNode <<< {fields: _} <$> objectParser
  where
  variableParser = string "$" *> name
  
  stringParser = charListToStringParser $ quotes $ many $ noneOf ['"']
    where
    charListToStringParser = map (fromCharArray <<< toUnfoldable)

  intParser =
    (regex "-?0\\d+" >>= \_ -> fail "Numbers cannot have leading zeros!")
      <|> regex "-?([1-9]\\d*|0)"

  floatParser = do
    val <- intParser
    (val <> _) <$> regex "(\\.\\d+|[Ee]-?\\d+|\\.\\d+|[Ee]-?\\d+)"

  booleanParser = (_ == "true") <$> (string "true" <|> string "false")

  listParser = bracket (many (whiteSpace *> (defer \_ -> value) <* whiteSpace))

  objectParser = braces (many (whiteSpace *> fieldParser <* whiteSpace))
    where
    fieldParser = do
      n <- name
      _ <- whiteSpace *> char ':' <* whiteSpace
      v <- value
      pure $ AST.ObjectFieldNode { name: n, value: v }
