module GraphQL.Type.Scalar where

import Prelude

import Data.Argonaut.Core as Json
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Number as Number
import Data.Maybe (Maybe(..))
import GraphQL.Language.AST as AST
import GraphQL.Type (ScalarType(..))

string :: ScalarType String
string = ScalarType { name, description, parseLiteral, parseValue, serialize }
  where
  name = "String"
  description = Just "Built in string scalar type."
  parseLiteral (AST.StringValueNode { value }) = pure value
  parseLiteral _ = Left "Expected string value node for input type String."
  parseValue = Json.caseJsonString (Left "Expected string JSON value.") pure
  serialize = Json.fromString

int :: ScalarType Int
int = ScalarType { name, description, parseLiteral, parseValue, serialize }
  where
  name = "Int"
  description = Just "Built in integer scalar type."
  parseLiteral (AST.IntValueNode { value }) =
    note "Expected integer value within the JavaScript Int bounds." $ Int.fromString value
  parseLiteral _ = Left "Expected integer value node for input type Int."
  parseValue = Json.caseJsonNumber (Left "Expected number JSON value.")
    (note "Expected integer value but got a floating point value" <<< Int.fromNumber)
  serialize = Json.fromNumber <<< Int.toNumber

float :: ScalarType Number
float = ScalarType { name, description, parseLiteral, parseValue, serialize }
  where
  name = "Float"
  description = Just "Built in float scalar type."
  parseLiteral (AST.FloatValueNode { value }) =
    note "Expected float value within the JavaScript number bounds." $ Number.fromString value
  parseLiteral _ = Left "Expected float value node for input type Float."
  parseValue = Json.caseJsonNumber (Left "Expected number JSON value.") pure
  serialize = Json.fromNumber

id :: ScalarType String
id = ScalarType { name, description, parseLiteral, parseValue, serialize }
  where
  name = "ID"
  description = Just "Built in ID scalar type, accepts both integers and strings."
  parseLiteral (AST.StringValueNode { value }) = pure value
  parseLiteral (AST.IntValueNode { value }) = pure value
  parseLiteral _ = Left "Expected string or integer value node for input type ID."
  parseValue = Json.caseJsonString (Left "Expected string or integer JSON value.") pure
  serialize = Json.fromString
