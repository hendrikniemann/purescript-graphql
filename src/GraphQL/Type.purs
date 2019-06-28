module GraphQL.Type where

import Prelude

import Data.Argonaut.Core as Json
import Data.Argonaut.Encode (extend)
import Data.Either (Either(..))
import Data.List (List)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (find, foldr, traverse)
import Data.Tuple (Tuple(..))
import GraphQL.Language.AST as AST
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Type.Row as Row
import Type.RowList as RL
import Unsafe.Coerce (unsafeCoerce)

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

newtype Argument a =
  Argument
    { description :: Maybe String
    , parseLiteral :: AST.ValueNode -> Either String a
    , parseValue :: Json.Json -> Either String a
    }

-- | A type class constraining the resolver arguments parameter to the supplied
-- | arguments declaration.
-- | E.g. if the provided args are of type `{ name: Argument String }` the
-- | resolvers second argument needs to be of type `{ name: String }`.
-- class ArgDeclarationToArgs
--   (decl :: # Type)
--   (args :: # Type)
--   | decl -> args, args -> decl

-- instance argDeclarationToArgsImpl
--   :: ( RowToList decl ldecl
--      , ConvertDeclArgs ldecl largs
--      , ListToRow largs args )
--   => ArgDeclarationToArgs decl args

-- class ConvertDeclArgs
--   (ldecl :: RowList)
--   (largs :: RowList)
--   | ldecl -> largs, largs -> ldecl

-- instance convertDeclArgsNil :: ConvertDeclArgs Nil Nil

-- instance convertDeclArgsCons :: ConvertDeclArgs ldecl largs
--   => ConvertDeclArgs (Cons k (Argument a) ldecl) (Cons k a largs)

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
    , fields :: Map String (ExecutableField a)
    }

instance outputTypeObjectType :: OutputType ObjectType where
  serialize (ObjectType o) (Just (AST.SelectionSetNode { selections })) val =
    foldr extend Json.jsonEmptyObject <$> traverse (serializeField o.fields val) selections
  serialize _ _ _ = Left "Missing subselection on object type."

instance showObjectType :: Show (ObjectType a) where
  show (ObjectType { name }) = "type " <> name

serializeField ::
  forall a. Map String (ExecutableField a) -> a -> AST.SelectionNode -> Either String (Tuple String Json.Json)
serializeField fields val node@(AST.FieldNode fld) =
  let (AST.NameNode { value: name }) = fld.name
      (AST.NameNode { value: alias }) = fromMaybe fld.name fld.alias
  in case lookup name fields of
    Just (ExecutableField { execute }) ->
      execute val node >>= (Tuple alias >>> pure)
    Nothing -> Left ("Unknown field `" <> name <> "` in selection.")
serializeField _ _ _ = Left "Error!"

newtype ExecutableField a =
  ExecutableField { execute :: a -> AST.SelectionNode -> Either String Json.Json }

newtype Field a argsd argsp =
  Field
    { name :: String
    , description :: Maybe String
    , args :: Record argsd
    , serialize :: AST.SelectionNode -> Record argsp -> a -> Either String Json.Json
    }

arg :: forall t a n. InputType t => IsSymbol n => t a -> SProxy n -> Tuple (SProxy n) (Argument a)
arg t name =
    Tuple (SProxy :: _ n) $
      Argument { description: Nothing, parseLiteral: parseL, parseValue: parseV }
  where
    parseL = parseLiteral t
    parseV = parseValue t

field :: forall t a. OutputType t => String -> t a -> Field a () ()
field name t = Field { name, description: Nothing, args: {}, serialize: serialize' }
  where
    serialize' (AST.FieldNode node) _ val = serialize t node.selectionSet val
    serialize' _ _ _ = Left "Somehow obtained non FieldNode for field serialisation..."

objectType :: forall a. String -> ObjectType a
objectType name =
  ObjectType { name, description: Nothing, fields: empty }

-- * Combinator operators

class Describe a where
  describe :: a -> String -> a

infixl 8 describe as .>

instance describeObjectType :: Describe (ObjectType a) where
  describe (ObjectType config) s =
    ObjectType (config { description = Just s })

instance describeField :: Describe (Field a argsd argsp) where
  describe (Field config) s =
    Field (config { description = Just s })

infixl 5 withField as :>

withField :: forall a argsd argsp.
  ArgsDefToArgsParam argsd argsp =>
  ObjectType a ->
  Field a argsd argsp ->
  ObjectType a
withField (ObjectType objectConfig) fld@(Field { name }) =
  ObjectType $ objectConfig { fields = insert name (makeExecutable fld) objectConfig.fields }
    where
      makeExecutable :: Field a argsd argsp -> ExecutableField a
      makeExecutable (Field { args, serialize: serialize' }) = ExecutableField { execute: execute' }
        where
          execute' :: a -> AST.SelectionNode -> Either String Json.Json
          execute' val node@(AST.FieldNode { arguments: argumentNodes }) = do
            argumentValues <- argsFromDefinition argumentNodes args
            serialize' node argumentValues val
          execute' _ _ = Left "Unexpected non field node field execution..."

-- | After we have extracted the row list proxies we can use this class to write implementations
-- | that match based on the proxies.
class ArgsDefToArgsParam (argsd :: # Type) (argsp :: # Type)
    | argsd -> argsp
    , argsp -> argsd where
      argsFromDefinition ::
        List AST.ArgumentNode ->
        Record argsd ->
        Either String (Record argsp)

instance argsDefToArgsParamImpl ::
  ( RL.RowToList argsd largsd
  , RL.RowToList argsp largsp
  , ArgsFromRows largsd largsp argsd argsp
  , RL.ListToRow largsd argsd
  , RL.ListToRow largsp argsp ) => ArgsDefToArgsParam argsd argsp where
    argsFromDefinition = argsFromRows (RLProxy :: RLProxy largsd) (RLProxy :: RLProxy largsp)

-- Without the RowList we cannot match on the empty list
class ArgsFromRows
  (largsd :: RL.RowList)
  (largsp :: RL.RowList)
  (argsd :: # Type)
  (argsp :: # Type)
  where
    argsFromRows ::
      RLProxy largsd ->
      RLProxy largsp ->
      List AST.ArgumentNode ->
      Record argsd ->
      Either String (Record argsp)

-- Instance for when the resolver function expects an argument
instance argsFromRowsCons ::
  ( Row.Cons l (Argument a) targsd argsd
  , Row.Cons l a targsp argsp
  , Row.Lacks l targsd
  , Row.Lacks l targsp
  , ArgsFromRows ltargsd ltargsp targsd targsp
  , IsSymbol l ) =>
    ArgsFromRows
      (RL.Cons l (Argument a) ltargsd)
      (RL.Cons l a ltargsp)
      argsd
      argsp
        where
    argsFromRows argsdProxy argspProxy nodes argsd =
      let key = SProxy :: SProxy l
          Argument argConfig = Record.get key argsd
          tailArgsDef = Record.delete key argsd :: Record targsd
          nameEquals (AST.ArgumentNode { name: AST.NameNode { value } }) =
            value == reflectSymbol key
          argumentNode = find nameEquals nodes
          -- Extract the argument value if the node can be found. If not use null value
          -- TODO: Implement default values where we would try to use the default value first
          valueNode = maybe AST.NullValueNode (\(AST.ArgumentNode { value }) -> value) argumentNode
          tail = argsFromRows (RLProxy :: RLProxy ltargsd) (RLProxy :: RLProxy ltargsp) nodes tailArgsDef
      in Record.insert key <$> (argConfig.parseLiteral valueNode) <*> tail

-- Instance for all other cases
else instance argsFromRowsNil :: ArgsFromRows RL.Nil RL.Nil argsd argsp where
  argsFromRows _ _ _ _ = pure $ unsafeCoerce {}

withArgument :: forall a arg n argsdold argsdnew argspold argspnew.
  IsSymbol n =>
  Row.Cons n (Argument arg) argsdold argsdnew =>
  Row.Cons n arg argspold argspnew =>
  Row.Lacks n argsdold =>
  Row.Lacks n argspold =>
  ArgsDefToArgsParam argsdold argspold =>
  ArgsDefToArgsParam argsdnew argspnew =>
  Field a argsdold argspold ->
  Tuple (SProxy n) (Argument arg) ->
  Field a argsdnew argspnew
withArgument (Field fieldConfig) (Tuple proxy argument) =
  Field $ fieldConfig { args = args', serialize = serialize' }
    where
      args' :: Record argsdnew
      args' = Record.insert proxy argument fieldConfig.args

      serialize' :: AST.SelectionNode -> Record argspnew -> a -> Either String Json.Json
      serialize' node argsnew val = fieldConfig.serialize node (Record.delete proxy argsnew) val

infixl 7 withArgument as ?>

withResolver :: forall a b argsd argsp. Field a argsd argsp -> (Record argsp -> b -> a) -> Field b argsd argsp
withResolver (Field fieldConfig) resolver =
  Field $ fieldConfig { serialize = serialize' }
    where 
      serialize' node args val = fieldConfig.serialize node args (resolver args val)

infixl 6 withResolver as !>

{-
data IntrospectionType
  =

data IntrospectionArgument
  = IntrospectionArgument { name :: String, description :: String, type :: Unit -> IntrospectionType }

newtype GraphQLType = GraphQLType
  { inspect :: Unit ->  }

stringArg :: String -> Maybe String -> Argument String

