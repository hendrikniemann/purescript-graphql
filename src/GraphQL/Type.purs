module GraphQL.Type where

import Prelude

import Data.Argonaut.Core as Json
import Data.Argonaut.Encode (extend)
import Data.Either (Either(..))
import Data.Map (Map, lookup)
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
    { serialize :: a -> AST.SelectionSetNode -> Either String Json.Json
    }

instance outputTypeObjectType :: OutputType ObjectType where
  serialize (ObjectType o) (Just selectionSet) val = o.serialize val selectionSet
  serialize _ _ _ = Left "Missing subselection on object type."

newtype Field a = Field { serialize :: a -> AST.SelectionNode -> Either String Json.Json }

arg :: forall t a n. InputType t => IsSymbol n => t a -> SProxy n -> String -> Argument n a
arg t name desc = Argument { name, description: Just desc, parseLiteral: parseL, parseValue: parseV }
  where
    parseL = parseLiteral t
    parseV = parseValue t

field :: forall t a b. OutputType t => t b -> (a -> b) -> Field a
field t resolver = Field { serialize: serialize' }
  where
    serialize' a (AST.FieldNode node) = serialize t node.selectionSet (resolver a)
    serialize' _ _ = Left "Somehow obtained non FieldNode for field serialisation..."

objectType :: forall a. Map String (Field a) -> ObjectType a
objectType fields = ObjectType { serialize: serialize' }
  where
    serialize' :: a -> AST.SelectionSetNode -> Either String Json.Json
    serialize' val (AST.SelectionSetNode { selections }) =
      foldr extend Json.jsonEmptyObject <$> traverse (serializeField val) selections

    serializeField :: a -> AST.SelectionNode -> Either String (Tuple String Json.Json)
    serializeField val node@(AST.FieldNode fld) =
      let (AST.NameNode { value: name }) = fld.name
          (AST.NameNode { value: alias }) = fromMaybe fld.name fld.alias
      in case lookup name fields of
        Just (Field { serialize: serializeField' }) ->
          serializeField' val node >>= (Tuple alias >>> pure)
        Nothing -> Left ("Unknown field `" <> name <> "` in selection.")
    serializeField _ _ = Left "Error!"

{-
data IntrospectionType
  =

data IntrospectionArgument
  = IntrospectionArgument { name :: String, description :: String, type :: Unit -> IntrospectionType }

newtype GraphQLType = GraphQLType
  { inspect :: Unit ->  }

stringArg :: String -> Maybe String -> Argument String


userType :: ObjectType User
userType =
  objectType "User"
    .> "My user type."
    :> field (SProxy :: SProxy "id") string
    :> field (SProxy :: SProxy "friends") int
      ?> required $ arg (SProxy :: SProxy "max") int "Maximum number of friends"
      !> \{ parent, args } -> do
          friends <- loadFriends parent.id
          pure $ case args.max of
            Just m -> take m friends
            Nothing -> friends
