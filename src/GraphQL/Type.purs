module GraphQL.Type where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core as Json
import Data.Either (Either(..), note)
import Data.Enum (class Enum, enumFromTo)
import Data.List (List, fromFoldable)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (class Traversable, find, traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import GraphQL.Execution.Result (Result(..))
import GraphQL.Language.AST as AST
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Type.Row as Row
import Type.RowList as RL
import Unsafe.Coerce (unsafeCoerce)

-- | The schema contains the central entry points for GraphQL queries.
newtype Schema m a = Schema { query :: ObjectType m a }

type VariableMap = Map String Json.Json

-- | The input type class is used for all GraphQL types that can be used as input types.
class InputType t where
  input :: forall a. t a -> Maybe AST.ValueNode -> VariableMap -> Either String a

-- | The output type class is used for all GraphQL types that can be used as output types.
-- | It has instances for GraphQL types that can resolve a value within a certain monad error.
class MonadError Error m <= OutputType m t where
  output :: forall a. t a -> Maybe AST.SelectionSetNode -> VariableMap -> m a -> m Result

-- | Scalar types are the leaf nodes of a GraphQL schema. Scalar types can be serialised into JSON
-- | or obtained from a JSON when read from the variables provided by a query. Furthermore values
-- | can be read from literals in a GraphQL document. To create a scalar type a name and optionally
-- | a description need to be provided.
-- | The functions `parseLiteral`, `parseValue` and `serialize` are used to convert between the
-- | GraphQL transport formats and PureScript representations.
-- |
-- | The GraphQL PureScript implementation comes with the four default scalars defined in the
-- | specification.
newtype ScalarType a =
  ScalarType
    { name :: String
    , description :: Maybe String
    , parseLiteral :: AST.ValueNode -> Either String a
    , parseValue :: Json.Json -> Either String a
    , serialize :: a -> Json.Json
    }

instance inputTypeScalarType :: InputType ScalarType where
  input (ScalarType config) (Just node) variables = case node of
    (AST.VariableNode { name: AST.NameNode { value }}) -> do
      json <- note ("Required variable `" <> value <> "` was not provided.") $
        lookup value variables
      config.parseValue json
    _ -> config.parseLiteral node
  input _ Nothing _ = Left "Must provide value for required scalar."

instance outputTypeScalarType :: (MonadError Error m) => OutputType m ScalarType where
  output (ScalarType s) Nothing _ val = ResultLeaf <$> map s.serialize val
  output _ _ _ _ = pure $ ResultError "Invalid subselection on scalar type!"

instance showScalarType :: Show (ScalarType a) where
  show (ScalarType { name }) = "scalar " <> name

newtype ObjectType m a =
  ObjectType
    { name :: String
    , description :: Maybe String
    , fields :: Map String (ExecutableField m a)
    }

instance outputTypeObjectType :: (MonadError Error m) => OutputType m (ObjectType m) where
  output (ObjectType o) (Just (AST.SelectionSetNode { selections })) variables val =
    ResultObject <$> traverse serializeField selections
      where
        serializeField :: AST.SelectionNode -> m (Tuple String Result)
        serializeField node@(AST.FieldNode fld) =
          let (AST.NameNode { value: name }) = fld.name
              (AST.NameNode { value: alias }) = fromMaybe fld.name fld.alias
          in case lookup name o.fields of
            Just (ExecutableField { execute }) ->
              Tuple alias <$> execute val node variables
            Nothing -> pure $ Tuple alias $ ResultError ("Unknown field `" <> name <> "` in selection.")
        -- TODO: This branch needs fixing. It is for fragment spreads and so on. Maybe normalise
        --       the query first and completely eliminate the branch...
        serializeField _ = pure $ Tuple "unknown" $ ResultError "Unexpected fragment spread!"
  output _ _ _ _ = pure $ ResultError "Missing subselection on object type."

instance showObjectType :: Show (ObjectType m a) where
  show (ObjectType { name }) = "type " <> name

-- | The executable field loses the information about it's arguments types. This is needed to add it
-- | to the map of arguments of the object type. The execute function will extract the arguments of
-- | the field from the AST.
newtype ExecutableField m a =
  ExecutableField { execute :: m a -> AST.SelectionNode -> VariableMap -> m Result }

-- | Object types can have fields. Fields are constructed using the `field` function from this
-- | module and added to object types using the `:>` operator.
newtype Field m a argsd argsp =
  Field
    { name :: String
    , description :: Maybe String
    , args :: Record argsd
    , serialize :: AST.SelectionNode -> VariableMap -> Record argsp -> m a -> m Result
    }

-- | Fields can have multiple arguments. Arguments are constructed using the `arg` function from
-- | this module and the `?>` operator.
newtype Argument a =
  Argument
    { description :: Maybe String
    , resolveValue :: Maybe AST.ValueNode -> VariableMap -> Either String a
    }

newtype EnumType a =
  EnumType
    { name :: String
    , description :: Maybe String
    , values :: Array (EnumValue a)
    }

newtype EnumValue a =
  EnumValue
    { name :: String
    , description :: Maybe String
    , value :: a
    , isValue :: a -> Boolean }

instance inputTypeEnumType :: InputType EnumType where
  input (EnumType config) (Just node) variables =
    let lookupName name =
          note ("Unknown enum value `" <> name <> "` for type `" <> config.name <> "`.") $
            find (\(EnumValue val) -> val.name == name) config.values
    in case node of
      AST.VariableNode { name: AST.NameNode { value: name } } -> do
        json <- note ("Required variable `" <> name <> "` was not provided.") $
          lookup name variables
        EnumValue val <- Json.caseJsonString (Left "Enum values must be strings.") lookupName json
        pure val.value

      AST.EnumValueNode { name: AST.NameNode { value: name } } -> do
        EnumValue val <- lookupName name
        pure val.value

      _ -> Left "Unexpected non-enum value node for enum value."

  input _ _ _ = Left "Missing value for required argument."

instance outputTypeEnumType :: MonadError Error m => OutputType m EnumType where
  output :: forall a. EnumType a -> Maybe AST.SelectionSetNode -> VariableMap -> m a -> m Result
  output (EnumType config) Nothing _ mValue = do
    value <- mValue
    case find (\(EnumValue { isValue }) -> isValue value) config.values of
      Nothing ->
        throwError $ error "Could not select enum value representation from provided value."

      Just (EnumValue val) ->
        pure $ ResultLeaf $ Json.fromString val.name

  output _ _ _ _ = pure $ ResultError "Invalid subselection on enum type!"

-- | Creates an empty object type with the given name. Fields can be added to the object type using
-- | the `:>` operator from this module.
objectType :: forall m a. String -> ObjectType m a
objectType name =
  ObjectType { name, description: Nothing, fields: empty }

-- | Create a new field for an object type. This function is typically used together with the
-- | `:>` operator that automatically converts the field into an executable field.
field :: forall m t a. OutputType m t => MonadError Error m => String -> t a -> Field m a () ()
field name t = Field { name, description: Nothing, args: {}, serialize }
  where
    serialize (AST.FieldNode node) variables _ val = output t node.selectionSet variables val
    serialize _ _ _ _ = pure $ ResultError "Obtained non FieldNode for field serialisation."

-- | Create a new field for an object type that is a list. You can return any `Foldable` in the
-- | resolver.
listField :: forall m f t a.
  MonadError Error m =>
  Traversable f =>
  OutputType m t =>
  String ->
  t a ->
  Field m (f a) () ()
listField name t = Field { name, description: Nothing, args: {}, serialize }
  where
    serialize (AST.FieldNode node) variables _ mVals = do
      vals <- mVals
      ResultList <$> fromFoldable <$> traverse (output t node.selectionSet variables) (pure <$> vals)

    serialize _ _ _ _ =
      pure $ ResultError "Obtained non FieldNode for field serialisation."

-- | Create a new field for an object type that is optional (i.e. it can be null). The resolver
-- | must now return a `Maybe`.
nullableField :: forall m t a.
  MonadError Error m =>
  OutputType m t =>
  String ->
  t a ->
  Field m (Maybe a) () ()
nullableField name t = Field { name, description: Nothing, args: {}, serialize }
  where
    serialize (AST.FieldNode node) variables _ mValue = do
      value <- mValue
      case value of
        Nothing ->
          pure $ ResultNullable Nothing

        Just val ->
          ResultNullable <$> Just <$> output t node.selectionSet variables (pure val)

    serialize _ _ _ _ =
      pure $ ResultError "Obtained non FieldNode for field serialisation."

-- | Create a tuple with a given name and a plain argument of the given type.
-- | The name argument tuple can then be used to be added to a field using the `?>` operator.
arg :: forall t a n. InputType t => IsSymbol n => t a -> SProxy n -> Tuple (SProxy n) (Argument a)
arg t name =
    Tuple (SProxy :: _ n) $
      Argument { description: Nothing, resolveValue: input t }

-- * Combinator operators

-- | The describe type class is used to allow adding descriptions to various parts of a GraphQL
-- | schema that can have a description. The `.>` operator can be used with the GraphQL object
-- | type DSL to easily add descriptions in various places.
-- |
-- | *Example:*
-- | ```purescript
-- | objectType "User"
-- |   .> "A user of the product."
-- |   :> "id" Scalar.id
-- |     .> "A unique identifier for this user."
-- | ```
class Describe a where
  describe :: a -> String -> a

infixl 8 describe as .>

instance describeObjectType :: Describe (ObjectType m a) where
  describe (ObjectType config) s = ObjectType (config { description = Just s })

instance describeField :: Describe (Field m a argsd argsp) where
  describe (Field config) s = Field (config { description = Just s })

instance describeArgument :: Describe (Argument a) where
  describe (Argument config) s = Argument (config { description = Just s })

instance describeEnumType :: Describe (EnumType a) where
  describe (EnumType config) s = EnumType ( config { description = Just s })

instance describeEnumValue :: Describe (EnumValue a) where
  describe (EnumValue config) s = EnumValue ( config { description = Just s })

-- | The `withField` function is used to add fields to an object type.
-- |
-- | When added to an object type the information about the field's argument are hidden in the
-- | closure by being converted to the `ExecutableField` type.
withField :: forall m a argsd argsp.
  ArgsDefToArgsParam argsd argsp =>
  MonadError Error m =>
  ObjectType m a ->
  Field m a argsd argsp ->
  ObjectType m a
withField (ObjectType objectConfig) fld@(Field { name }) =
  ObjectType $ objectConfig { fields = insert name (makeExecutable fld) objectConfig.fields }
    where
      makeExecutable :: Field m a argsd argsp -> ExecutableField m a
      makeExecutable (Field { args, serialize: serialize' }) = ExecutableField { execute: execute' }
        where
          execute' :: m a -> AST.SelectionNode -> VariableMap -> m Result
          execute' val node@(AST.FieldNode { arguments: argumentNodes }) variables =
            case argsFromDefinition argumentNodes variables args of
              Right argumentValues -> serialize' node variables argumentValues val
              Left err -> pure (ResultError err)
          execute' _ _ _ = pure (ResultError "Unexpected non field node field execution...")

-- | Add a field to an object type.
-- |
-- | *Example:*
-- | ```purescript
-- | objectType "Query"
-- |   :> field "hello" Scalar.string
-- | ```
infixl 5 withField as :>

-- | A type class that contrains the relationship between defined arguments and the _argument_
-- | parameter of a resolver.
class ArgsDefToArgsParam (argsd :: # Type) (argsp :: # Type)
    | argsd -> argsp
    , argsp -> argsd where
      argsFromDefinition ::
        List AST.ArgumentNode ->
        VariableMap ->
        Record argsd ->
        Either String (Record argsp)

instance argsDefToArgsParamImpl ::
  ( RL.RowToList argsd largsd
  , RL.RowToList argsp largsp
  , ArgsFromRows largsd largsp argsd argsp
  , RL.ListToRow largsd argsd
  , RL.ListToRow largsp argsp ) => ArgsDefToArgsParam argsd argsp where
    argsFromDefinition = argsFromRows (RLProxy :: RLProxy largsd) (RLProxy :: RLProxy largsp)

-- | ArgsDefToArgsParam that works on row lists for matching implementations
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
      VariableMap ->
      Record argsd ->
      Either String (Record argsp)

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
    argsFromRows argsdProxy argspProxy nodes variables argsd =
      let key = SProxy :: SProxy l
          Argument argConfig = Record.get key argsd
          tailArgsDef = Record.delete key argsd :: Record targsd
          nameEquals (AST.ArgumentNode { name: AST.NameNode { value } }) =
            value == reflectSymbol key
          argumentNode = find nameEquals nodes
          -- Extract the argument value if the node can be found. If not use null value
          -- TODO: Implement default values where we would try to use the default value first
          valueNode = map (\(AST.ArgumentNode { value }) -> value) argumentNode
          tail =
            argsFromRows
              (RLProxy :: RLProxy ltargsd)
              (RLProxy :: RLProxy ltargsp)
              nodes
              variables
              tailArgsDef
      in Record.insert key <$> (argConfig.resolveValue valueNode variables) <*> tail

else instance argsFromRowsNil :: ArgsFromRows RL.Nil RL.Nil argsd argsp where
  argsFromRows _ _ _ _ _ = pure $ unsafeCoerce {}

-- | The `withArgument` function adds an argument to a field. The argument can then be used in
-- | resolvers that are added to the field. The old resolver of the field is still valid until it is
-- | overwritten by `withResolver.`
withArgument :: forall m a arg n argsdold argsdnew argspold argspnew.
  IsSymbol n =>
  Row.Cons n (Argument arg) argsdold argsdnew =>
  Row.Cons n arg argspold argspnew =>
  Row.Lacks n argsdold =>
  Row.Lacks n argspold =>
  ArgsDefToArgsParam argsdold argspold =>
  ArgsDefToArgsParam argsdnew argspnew =>
  Field m a argsdold argspold ->
  Tuple (SProxy n) (Argument arg) ->
  Field m a argsdnew argspnew
withArgument (Field fieldConfig) (Tuple proxy argument) =
  Field $ fieldConfig { args = args', serialize = serialize' }
    where
      args' :: Record argsdnew
      args' = Record.insert proxy argument fieldConfig.args

      serialize' ::
        AST.SelectionNode ->
        VariableMap ->
        Record argspnew ->
        m a ->
        m Result
      serialize' node variables argsnew val =
        fieldConfig.serialize node variables (Record.delete proxy argsnew) val

-- | Adds an argument to a field that can be used in the resolver.
-- |
-- | *Example:*
-- | ```purescript
-- | objectType "Query"
-- |   :> field "hello" Scalar.string
-- |     ?> arg (SProxy :: SProxy "name") Scalar.string
-- |     !> (\{ name } parent -> "Hello " <> name <> "!")
-- | ```
infixl 7 withArgument as ?>

-- | The `withResolver` function adds a resolver to a field.
-- |
-- | _Note: When used multiple times on a field the resolvers are chained in front of each other._
withResolver :: forall m a b argsd argsp.
  MonadError Error m =>
  Field m a argsd argsp ->
  (Record argsp -> m b -> m a) ->
  Field m b argsd argsp
withResolver (Field fieldConfig) resolver =
  Field $ fieldConfig { serialize = serialize }
    where
      serialize :: AST.SelectionNode -> VariableMap -> Record argsp -> m b -> m Result
      serialize node variables args val =
        fieldConfig.serialize node variables args (resolver args val)

-- | Add a resolver to a field that receives the arguments in a record and the parent value.
-- |
-- | *Example:*
-- | ```purescript
-- | objectType "Query"
-- |   :> field "hello" Scalar.string
-- |     !> (\_ parent -> "Hello " <> parent.name <> "!")
-- | ```
infixl 6 withResolver as !>

-- | Take a bounded enum and infer an enum type from that value using it's show instance to
-- | represent the enum string. The show function cannot return values that are not valid GraphQL
-- | indentifier strings. For practical reasons this is not checked at compile time. Please refer to
-- | the tutorial for best practices around enum values.
enumType :: forall a. Bounded a => Enum a => Show a => String -> EnumType a
enumType name = EnumType { name, description: Nothing, values }
  where
    values = enumFromTo top bottom <#> \value ->
      EnumValue { name: show value, description: Nothing, value, isValue: eq value }
