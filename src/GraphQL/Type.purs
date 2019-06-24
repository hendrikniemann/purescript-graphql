module GraphQL.Type where

import Prelude

import Data.Argonaut.Core as Json
import Data.Argonaut.Encode (extend)
import Data.Either (Either(..))
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Traversable (foldr, traverse)
import Data.Tuple (Tuple(..))
import GraphQL.Language.AST as AST

class InputType t where
  parseLiteral :: forall a. t a -> AST.ValueNode -> Either String a
  parseValue :: forall a. t a -> Json.Json -> Either String a

class OutputType t where
  serialize :: forall a. t a -> Maybe AST.SelectionSetNode -> a -> Either String Json.Json

newtype ScalarType a =
  ScalarType
    { name :: String
    , description :: Maybe String
    , parseLiteral :: AST.ValueNode -> Either String a
    , parseValue :: Json.Json -> Either String a
    , serialize :: a -> Json.Json
    }

instance inputTypeScalarType :: InputType ScalarType where
  parseLiteral (ScalarType s) = s.parseLiteral
  parseValue (ScalarType s) = s.parseValue

instance outputTypeScalarType :: OutputType ScalarType where
  serialize (ScalarType s) Nothing val = pure $ s.serialize val
  serialize _ _ _ = Left "Invalid subselection on scalar type!"

instance showScalarType :: Show (ScalarType a) where
  show (ScalarType { name, description }) = case description of
    Just desc -> "\"\"\"\n" <> desc <> "\n\"\"\"\nscalar " <> name
    Nothing -> "scalar " <> name

newtype Argument n a =
  Argument
    { name :: SProxy n
    , description :: Maybe String
    , parseLiteral :: AST.ValueNode -> Either String a
    , parseValue :: Json.Json -> Either String a
    }

-- parseArgs :: forall args argsp. Record args -> List.List AST.ArgumentNode -> Json.Json -> Record argsp
-- parseArgs args nodes variables = ?mapArgs parseArg args nodes
--   where
--     parseArg :: forall a n. Argument n a -> AST.ArgumentNode -> Either String a
--     parseArg (Argument arg) (AST.ArgumentNode { value }) = case value of
--       AST.VariableNode { name } -> map arg.parseValue $ variables .: name
--       v -> arg.parseLiteral v

--     findArg :: List.List AST.ArgumentNode -> String -> Either String a
--     findArg list name = List.find 

-- serializeField :: Field n a args AST.FieldNode -> String

newtype Schema a = Schema { query :: ObjectType a }

newtype ObjectType a =
  ObjectType
    { name :: String
    , description :: Maybe String
    , fields :: Map String (Field a)
    }

instance outputTypeObjectType :: OutputType ObjectType where
  serialize (ObjectType o) (Just (AST.SelectionSetNode { selections })) val =
    foldr extend Json.jsonEmptyObject <$> traverse (serializeField o.fields val) selections
  serialize _ _ _ = Left "Missing subselection on object type."

instance showObjectType :: Show (ObjectType a) where
  show (ObjectType { name }) = "type " <> name

serializeField ::
  forall a. Map String (Field a) -> a -> AST.SelectionNode -> Either String (Tuple String Json.Json)
serializeField fields val node@(AST.FieldNode fld) =
  let (AST.NameNode { value: name }) = fld.name
      (AST.NameNode { value: alias }) = fromMaybe fld.name fld.alias
  in case lookup name fields of
    Just (Field { serialize: serializeField' }) ->
      serializeField' val node >>= (Tuple alias >>> pure)
    Nothing -> Left ("Unknown field `" <> name <> "` in selection.")
serializeField _ _ _ = Left "Error!"

newtype Field a =
  Field
    { name :: String
    , description :: Maybe String
    , serialize :: a -> AST.SelectionNode -> Either String Json.Json
    }

arg :: forall t a n. InputType t => IsSymbol n => t a -> SProxy n -> String -> Argument n a
arg t name desc = Argument { name, description: Just desc, parseLiteral: parseL, parseValue: parseV }
  where
    parseL = parseLiteral t
    parseV = parseValue t

field :: forall t a. OutputType t => String -> t a -> Field a
field name t = Field { name, description: Nothing, serialize: serialize' }
  where
    serialize' a (AST.FieldNode node) = serialize t node.selectionSet a
    serialize' _ _ = Left "Somehow obtained non FieldNode for field serialisation..."

objectType :: forall a. String -> ObjectType a
objectType name =
  ObjectType { name, description: Nothing, fields: empty }

-- ** Nice operator magic ** -

class Describe a where
  describe :: a -> String -> a

infixl 7 describe as .>

instance describeObjectType :: Describe (ObjectType a) where
  describe (ObjectType config) s =
    ObjectType (config { description = Just s })

instance describeField :: Describe (Field a) where
  describe (Field config) s =
    Field (config { description = Just s })

withField :: forall a. ObjectType a -> Field a -> ObjectType a
withField (ObjectType objectConfig) fld@(Field { name }) =
  ObjectType $ objectConfig { fields = insert name fld objectConfig.fields }

infixl 5 withField as :>

withResolver :: forall a b. Field a -> (b -> a) -> Field b
withResolver (Field fieldConfig) resolver =
  Field $ fieldConfig { serialize = fieldConfig.serialize <<< resolver }

infixl 6 withResolver as !>

{-
data IntrospectionType
  =

data IntrospectionArgument
  = IntrospectionArgument { name :: String, description :: String, type :: Unit -> IntrospectionType }

newtype GraphQLType = GraphQLType
  { inspect :: Unit ->  }

stringArg :: String -> Maybe String -> Argument String

