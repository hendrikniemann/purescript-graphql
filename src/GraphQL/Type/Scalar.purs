module GraphQL.Type.Scalar where

import Prelude

import Data.Argonaut.Core as Json
import Data.Either (Either(..), note)
import Data.Int (fromNumber, fromString, toNumber)
import Data.Maybe (Maybe(..))
import GraphQL.Language.AST as AST
import GraphQL.Type (ScalarType(..))

string :: ScalarType String
string = ScalarType { name, description, parseLiteral, parseValue, serialize }
  where
  name = "String"
  description = Just "Built in string scalar type."
  parseLiteral (AST.StringValueNode { value }) = pure value
  parseLiteral _ = Left "Expected input string."
  parseValue = Json.caseJsonString (Left "Expected string JSON value.") pure
  serialize = Json.fromString

int :: ScalarType Int
int = ScalarType { name, description, parseLiteral, parseValue, serialize }
  where
  name = "Int"
  description = Just "Built in integer scalar type."
  parseLiteral (AST.IntValueNode { value }) =
    note "Expected integer value within the JavaScript Int bounds." $ fromString value
  parseLiteral _ = Left "Expected integer node."
  parseValue = Json.caseJsonNumber (Left "Expected number JSON value.")
    (note "Expected integer value but got a floating point value" <<< fromNumber)
  serialize = Json.fromNumber <<< toNumber
