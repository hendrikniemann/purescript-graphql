module GraphQL.Type where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Enum (class Enum, enumFromTo)
import Data.List (List, fromFoldable, singleton, filter)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (class Traversable, find, traverse)
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect.Exception (Error, error, message)
import GraphQL.Execution.Result (Result(..))
import GraphQL.Language.AST as AST
import GraphQL.Language.AST.Util (nameFromNameNode)
import GraphQL.Type.Introspection.Datatypes as IntrospectionTypes
import Prim.Row (class Lacks, class Cons)
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Type.Row as Row
import Type.RowList as RL
import Unsafe.Coerce (unsafeCoerce)

-- | The schema contains the central entry points for GraphQL queries.
newtype Schema m a = Schema { query :: ObjectType m a, mutation :: Maybe (ObjectType m a) }

type ExecutionContext =
  { variables :: Map String Json.Json
  , fragments :: Map String AST.DefinitionNode
  }

-- | The type class for all GraphQL types
class GraphQLType t where
  introspect :: forall a. t a -> IntrospectionTypes.TypeIntrospection

-- | The input type class is used for all GraphQL types that can be used as input types.
class GraphQLType t <= InputType t where
  input :: forall a. t a -> Maybe AST.ValueNode -> ExecutionContext -> Either String a

-- | The output type class is used for all GraphQL types that can be used as output types.
-- | It has instances for GraphQL types that can resolve a value within a certain monad error.
class (MonadError Error m, GraphQLType t) <= OutputType m t where
  output :: forall a. t a -> Maybe AST.SelectionSetNode -> ExecutionContext -> m a -> m Result

-- | Scalar types are the leaf nodes of a GraphQL schema. Scalar types can be serialised into JSON
-- | or obtained from a JSON when read from the variables provided by a query. Furthermore values
-- | can be read from literals in a GraphQL document. To create a scalar type a name and optionally
-- | a description need to be provided.
-- | The functions `parseLiteral`, `parseValue` and `serialize` are used to convert between the
-- | GraphQL transport formats and PureScript representations.
-- |
-- | The GraphQL PureScript implementation comes with the five default scalars defined in the
-- | specification.
newtype ScalarType a =
  ScalarType
    { name :: String
    , description :: Maybe String
    , parseLiteral :: AST.ValueNode -> Either String a
    , parseValue :: Json.Json -> Either String a
    , serialize :: a -> Json.Json
    }

instance graphqlTypeScalarType :: GraphQLType ScalarType where
  introspect (ScalarType { name, description }) =
    IntrospectionTypes.TypeIntrospection
      { kind: IntrospectionTypes.Scalar
      , name: Just name
      , description
      , fields: Nothing
      , inputs: Nothing
      , enumValues: Nothing
      , ofType: Nothing
      , possibleTypes: Nothing
      }

instance inputTypeScalarType :: InputType ScalarType where
  input (ScalarType config) (Just node) execCtx = case node of
    (AST.VariableNode { name: AST.NameNode { value }}) -> do
      json <- note ("Required variable `" <> value <> "` was not provided.") $
        lookup value execCtx.variables
      config.parseValue json
    _ -> config.parseLiteral node
  input _ Nothing _ = Left "Must provide value for required scalar."

instance outputTypeScalarType :: (MonadError Error m) => OutputType m ScalarType where
  output (ScalarType s) Nothing _ val = ResultLeaf <$> map s.serialize val
  output _ _ _ _ = pure $ ResultError "Invalid subselection on scalar type!"

instance showScalarType :: Show (ScalarType a) where
  show (ScalarType { name }) = "scalar " <> name

newtype ObjectType m a =
  ObjectType (Unit ->
    { name :: String
    , fields :: Map String (ExecutableField m a)
    , introspection :: IntrospectionTypes.TypeIntrospection
    }
  )

instance graphqlTypeObjectType :: GraphQLType (ObjectType m) where
  introspect (ObjectType configFn) = (configFn unit).introspection

instance outputTypeObjectType :: (MonadError Error m) => OutputType m (ObjectType m) where
  output (ObjectType fno) (Just (AST.SelectionSetNode { selections })) execCtx mValue = do
    value <- mValue
    let serializeField node@(AST.FieldNode fld) =
          let (AST.NameNode { value: name }) = fld.name
              (AST.NameNode { value: alias }) = fromMaybe fld.name fld.alias
              o = fno unit
          in case lookup name o.fields of
            Just (ExecutableField { execute }) ->
              Tuple alias <$>
                (flip catchError (message >>> ResultError >>> pure) $
                  execute value node execCtx)
            Nothing ->
              if
                name == "__typename"
              then
                pure $ Tuple alias $ ResultLeaf $ Json.fromString o.name
              else
                pure $ Tuple alias $
                  ResultError ("Unknown field `" <> name <> "` on type `" <> o.name <> "`.")
        -- TODO: This branch needs fixing. It is for fragment spreads and so on. Maybe normalise
        --       the query first and completely eliminate the branch...
        serializeField _ = pure $ Tuple "unknown" $ ResultError "Unexpected fragment spread!"
    ResultObject <$> traverse serializeField (selections >>= collectField)
      where
        -- Flatten the selection node structure by spreading all fragments into the list
        collectField :: AST.SelectionNode -> List AST.SelectionNode
        collectField n@(AST.FieldNode _) = singleton n
        collectField (AST.InlineFragmentNode {
          selectionSet: (AST.SelectionSetNode { selections: s })
        }) = collectField =<< s
        collectField (AST.FragmentSpreadNode { name: AST.NameNode n }) =
          case lookup n.value execCtx.fragments of
            Just (AST.FragmentDefinitionNode {
              selectionSet: AST.SelectionSetNode { selections: s }
            }) -> collectField =<< s
            _ -> mempty


  output _ _ _ _ = pure $ ResultError "Missing subselection on object type."


instance showObjectType :: Show (ObjectType m a) where
  show (ObjectType config) = "type " <> (config unit).name

derive instance newtypeObjectType :: Newtype (ObjectType m a) _

instance lazyObjectType :: Lazy (ObjectType m a) where
  defer fn = ObjectType $ \_ -> unwrap (fn unit) unit

-- | The executable field loses the information about it's arguments types. This is needed to add it
-- | to the map of arguments of the object type. The execute function will extract the arguments of
-- | the field from the AST.
newtype ExecutableField m a =
  ExecutableField { execute :: a -> AST.SelectionNode -> ExecutionContext -> m Result }

-- | Object types can have fields. Fields are constructed using the `field` function from this
-- | module and added to object types using the `:>` operator.
newtype Field m a argsd argsp =
  Field
    { name :: String
    , description :: Maybe String
    , typeIntrospection :: Unit -> IntrospectionTypes.TypeIntrospection
    , argumentIntrospections :: Array IntrospectionTypes.InputValueIntrospection
    , args :: Record argsd
    , serialize :: AST.SelectionNode -> ExecutionContext -> Record argsp -> a -> m Result
    }

-- | Fields can have multiple arguments. Arguments are constructed using the `arg` function from
-- | this module and the `?>` operator.
newtype Argument a =
  Argument
    { description :: Maybe String
    , typeIntrospection :: Unit -> IntrospectionTypes.TypeIntrospection
    , resolveValue :: Maybe AST.ValueNode -> ExecutionContext -> Either String a
    }

derive instance newtypeArgument :: Newtype (Argument a) _

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

instance graphqlTypeEnumType :: GraphQLType EnumType where
  introspect (EnumType { name, description, values }) =
    IntrospectionTypes.TypeIntrospection
      { kind: IntrospectionTypes.Enum
      , name: Just name
      , description
      , fields: Nothing
      , inputs: Nothing
      , enumValues: pure $ values <#> \(EnumValue val) ->
          IntrospectionTypes.EnumValueIntrospection
            { name: val.name
            , description: val.description
            , deprecationReason: Nothing
            }
      , ofType: Nothing
      , possibleTypes: Nothing
      }

instance inputTypeEnumType :: InputType EnumType where
  input (EnumType config) (Just node) execContext =
    let lookupName name =
          note ("Unknown enum value `" <> name <> "` for type `" <> config.name <> "`.") $
            find (\(EnumValue val) -> val.name == name) config.values
    in case node of
      AST.VariableNode { name: AST.NameNode { value: name } } -> do
        json <- note ("Required variable `" <> name <> "` was not provided.") $
          lookup name execContext.variables
        EnumValue val <- Json.caseJsonString (Left "Enum values must be strings.") lookupName json
        pure val.value

      AST.EnumValueNode { name: AST.NameNode { value: name } } -> do
        EnumValue val <- lookupName name
        pure val.value

      _ -> Left "Unexpected non-enum value node for enum value."

  input _ _ _ = Left "Missing value for required argument."

instance outputTypeEnumType :: MonadError Error m => OutputType m EnumType where
  output :: forall a. EnumType a -> Maybe AST.SelectionSetNode -> ExecutionContext -> m a -> m Result
  output (EnumType config) Nothing _ mValue = do
    value <- mValue
    case find (\(EnumValue { isValue }) -> isValue value) config.values of
      Nothing ->
        throwError $ error "Could not select enum value representation from provided value."

      Just (EnumValue val) ->
        pure $ ResultLeaf $ Json.fromString val.name

  output _ _ _ _ = pure $ ResultError "Invalid subselection on enum type!"

newtype UnionType m a = UnionType (Unit ->
  { name :: String
  , description :: Maybe String
  , typeIntrospections :: Array IntrospectionTypes.TypeIntrospection
  , output :: Maybe AST.SelectionSetNode -> ExecutionContext -> m a -> m Result
  }
)

instance graphqlTypeUnionType :: GraphQLType (UnionType m) where
  introspect (UnionType config) = IntrospectionTypes.TypeIntrospection
    { kind: IntrospectionTypes.Union
    , name: Just (config unit).name
    , description: (config unit).description
    , fields: Nothing
    , inputs: Nothing
    , enumValues: Nothing
    , ofType: Nothing
    , possibleTypes: Just \_ -> (config unit).typeIntrospections
    }

-- Takes a string and only returns part of the selection set that matches the return type
-- TODO: Throw under certain cases, e.g. if there is a field selected in the root that is not
--       `__typename`
filterSelectionSet :: String -> AST.SelectionSetNode -> ExecutionContext -> AST.SelectionSetNode
filterSelectionSet typeName (AST.SelectionSetNode { selections }) ctx =
  AST.SelectionSetNode { selections: filter filterFn selections }
    where
      -- Only the __typename field is allowed on the root selection
      filterFn (AST.FieldNode { name: AST.NameNode { value }}) = value == "__typename"

      -- If we find a fragment we only include it into the selection if it matches the type
      filterFn (AST.FragmentSpreadNode { name: AST.NameNode { value }}) =
        case lookup value ctx.fragments of
          Just (AST.FragmentDefinitionNode { typeCondition: AST.SimpleNamedTypeNode { name }}) ->
            nameFromNameNode name == typeName
          _ -> false

      -- Same thing for inline fragment only here we don't have to lookup the fragment
      filterFn (AST.InlineFragmentNode { typeCondition: AST.SimpleNamedTypeNode { name } }) =
        nameFromNameNode name == typeName

instance outputTypeUnionType :: (MonadError Error m) => OutputType m (UnionType m) where
  output :: forall a. UnionType m a -> Maybe AST.SelectionSetNode -> ExecutionContext -> m a -> m Result
  output (UnionType config) = (config unit).output

union :: forall ctx def var. UnionDefinition def var ctx => String -> Record def -> UnionType ctx (Variant.Variant var)
union = createUnionFromDefinition

class UnionDefinition (defRow :: # Type) (varRow :: # Type) (ctx :: Type -> Type)
    | defRow -> varRow
    , varRow -> defRow where
      createUnionFromDefinition ::
        String ->
        Record defRow ->
        UnionType ctx (Variant.Variant varRow)

-- This instance is only to get the row list type from the record
instance unionDefinitionInstance ::
  ( RL.RowToList defRow defRowList
  , RL.RowToList varRow varRowList
  , UnionIntrospection defRowList defRow
  , UnionResolver defRowList varRowList defRow varRow ctx
  , MonadError Error ctx
  , RL.ListToRow defRowList defRow
  , RL.ListToRow varRowList varRow ) => UnionDefinition defRow varRow ctx where
    createUnionFromDefinition name defRecord =
      let
        typeIntrospections = unionIntrospection (RLProxy :: RLProxy defRowList) defRecord
        output selectionSetNode execCtx ctxValue = ctxValue >>=
          unionResolver
            (RLProxy :: RLProxy defRowList)
            (RLProxy :: RLProxy varRowList)
            defRecord
            selectionSetNode
            execCtx
      in
        UnionType $ \_ -> { name, description: Nothing, typeIntrospections, output }

class UnionIntrospection
  (defRowList :: RL.RowList)
  (defRow :: # Type)
  where
    unionIntrospection ::
      RLProxy defRowList ->
      Record defRow ->
      Array IntrospectionTypes.TypeIntrospection

instance unionIntrospectionCons ::
  ( Row.Cons l (ObjectType ctx a) defRowTail defRow
  , Row.Lacks l defRowTail
  , UnionIntrospection defRowListTail defRowTail
  , IsSymbol l
  ) =>
    UnionIntrospection (RL.Cons l (ObjectType ctx a) defRowListTail) defRow
        where
          unionIntrospection defRLProxy defRecord =
            let
              ObjectType configFn = Record.get (SProxy :: SProxy l) defRecord
              config = configFn unit
            in
              -- Recursively call function
              Array.cons config.introspection $
                unionIntrospection
                  (RLProxy :: RLProxy defRowListTail)
                  -- We will just read from this value
                  (unsafeCoerce defRecord :: Record defRowTail)

else instance unionIntrospectionNil :: UnionIntrospection RL.Nil defRow where
  unionIntrospection _ _ = []

class UnionResolver
  (defRowList :: RL.RowList)
  (varRowList :: RL.RowList)
  (defRow :: # Type)
  (varRow :: # Type)
  (ctx :: Type -> Type)
  where
    unionResolver ::
      RLProxy defRowList ->
      RLProxy varRowList ->
      Record defRow ->
      Maybe AST.SelectionSetNode ->
      ExecutionContext ->
      (Variant.Variant varRow) ->
      ctx Result

instance unionResolverCons ::
  ( Row.Cons l (ObjectType ctx a) defRowTail defRow
  , Row.Cons l a varRowTail varRow
  , Row.Lacks l defRowTail
  , Row.Lacks l varRowTail
  , UnionResolver defRowListTail varRowListTail defRowTail varRowTail ctx
  , MonadError Error ctx
  , IsSymbol l
  ) =>
    UnionResolver
      (RL.Cons l (ObjectType ctx a) defRowListTail)
      (RL.Cons l a varRowListTail)
      defRow
      varRow
      ctx
        where
          unionResolver ::
            RLProxy (RL.Cons l (ObjectType ctx a) defRowListTail) ->
            RLProxy (RL.Cons l a varRowListTail) ->
            Record defRow ->
            Maybe AST.SelectionSetNode ->
            ExecutionContext ->
            (Variant.Variant varRow) ->
            ctx Result
          unionResolver defRLProxy varRLProxy defRecord Nothing execCtx =
            const $ throwError $ error "Missing selection set for union type"
          unionResolver defRLProxy varRLProxy defRecord (Just selection) execCtx =
            let
              objectType@(ObjectType config) = Record.get (SProxy :: SProxy l) defRecord
              typename = (config unit).name
              filteredSelection = filterSelectionSet typename selection execCtx
              recResolver ::
                (Variant.Variant varRowTail) ->
                ctx Result
              recResolver =
                unionResolver
                  (RLProxy :: RLProxy defRowListTail)
                  (RLProxy :: RLProxy varRowListTail)
                  (unsafeCoerce defRecord :: Record defRowTail)
                  (Just selection)
                  execCtx
            in
                recResolver
                  # Variant.on (SProxy :: SProxy l)
                    (pure >>> output objectType (Just filteredSelection) execCtx)

else instance unionResolverNil :: Functor ctx => UnionResolver RL.Nil RL.Nil defRow varRow ctx where
  unionResolver _ _ _ _ _ = unsafeCoerce >>> Variant.case_

newtype InputObjectType a = InputObjectType (Unit ->
  { name :: String
  , description :: Maybe String
  , fieldIntrospection :: Array IntrospectionTypes.InputValueIntrospection
  , input :: Maybe AST.ValueNode -> ExecutionContext -> Either String a
  }
)

instance graphqlTypeInputObjectType :: GraphQLType InputObjectType where
  introspect (InputObjectType config) =
    IntrospectionTypes.TypeIntrospection
      { kind: IntrospectionTypes.InputObject
      , name: Just (config unit).name
      , description: (config unit).description
      , fields: Nothing
      , inputs: Just $ (config unit).fieldIntrospection
      , enumValues: Nothing
      , ofType: Nothing
      , possibleTypes: Nothing
      }

instance inputTypeInputObjectType :: InputType InputObjectType where
  input (InputObjectType config) = (config unit).input

derive instance newtypeInputObjectType :: Newtype (InputObjectType a) _

instance lazyInputObjectType :: Lazy (InputObjectType a) where
  defer fn = InputObjectType $ \_ -> unwrap (fn unit) unit

newtype InputField l a = InputField
  { name :: SProxy l
  , introspection :: IntrospectionTypes.InputValueIntrospection
  , input :: Maybe AST.ValueNode -> ExecutionContext -> Either String a
  }

-- * DSL Creator functions

-- | Creates an empty object type with the given name. Fields can be added to the object type using
-- | the `:>` operator from this module and the `field` functions. Object types should have at least
-- | one field to confirm to the GraphQL specification.
-- |
-- | *Example:*
-- | ```purescript
-- | queryType :: ObjectType Unit
-- | queryType = objectType "Query"
-- | ```
objectType :: forall m a. String -> ObjectType m a
objectType name =
  let
    introspection = IntrospectionTypes.TypeIntrospection
      { kind: IntrospectionTypes.Object
      , name: Just name
      , description: Nothing
      , fields: Just []
      , inputs: Nothing
      , enumValues: Nothing
      , ofType: Nothing
      , possibleTypes: Nothing
      }
  in
    ObjectType (\_ -> { name, fields: empty, introspection })

-- | Create a new field for an object type. This function is typically used together with the
-- | `:>` operator that automatically converts the field into an executable field.
field :: forall m t a. OutputType m t => MonadError Error m => String -> t a -> Field m a () ()
field name t =
  Field
    { name
    , description: Nothing
    , args: {}
    , serialize
    , argumentIntrospections: []
    , typeIntrospection }
      where
        serialize (AST.FieldNode node) execCtx _ val = output t node.selectionSet execCtx (pure val)
        serialize _ _ _ _ = pure $ ResultError "Obtained non FieldNode for field serialisation."

        typeIntrospection _ =
          IntrospectionTypes.TypeIntrospection
            { kind: IntrospectionTypes.NonNull
            , name: Nothing
            , description: Nothing
            , fields: Nothing
            , inputs: Nothing
            , enumValues: Nothing
            , ofType: Just $ \_ -> introspect t
            , possibleTypes: Nothing
            }

-- | Create a new field for an object type that is a list. You can return any `Traversable` from the
-- | resolver.
listField :: forall m f t a.
  MonadError Error m =>
  Traversable f =>
  OutputType m t =>
  String ->
  t a ->
  Field m (f a) () ()
listField name t =
  Field
    { name
    , description: Nothing
    , args: {}
    , serialize
    , argumentIntrospections: []
    , typeIntrospection }
      where
        serialize (AST.FieldNode node) execCtx _ vals = do
          ResultList <$> fromFoldable <$> traverse (output t node.selectionSet execCtx) (pure <$> vals)

        serialize _ _ _ _ =
          pure $ ResultError "Obtained non FieldNode for field serialisation."

        typeIntrospection _ =
          IntrospectionTypes.TypeIntrospection
            { kind: IntrospectionTypes.NonNull
            , name: Nothing
            , description: Nothing
            , fields: Nothing
            , enumValues: Nothing
            , inputs: Nothing
            , possibleTypes: Nothing
            , ofType: Just $ \_ ->
                IntrospectionTypes.TypeIntrospection
                  { kind: IntrospectionTypes.List
                  , name: Nothing
                  , description: Nothing
                  , fields: Nothing
                  , enumValues: Nothing
                  , inputs: Nothing
                  , ofType: Just $ \_ -> introspect t
                  , possibleTypes: Nothing
                  }
            }

-- | Create a new field for an object type that is optional (i.e. it can be null). The resolver
-- | must now return a `Maybe`.
nullableField :: forall m t a.
  MonadError Error m =>
  OutputType m t =>
  String ->
  t a ->
  Field m (Maybe a) () ()
nullableField name t =
  Field
    { name
    , description: Nothing
    , args: {}
    , serialize
    , argumentIntrospections: []
    , typeIntrospection }
      where
        serialize (AST.FieldNode node) execCtx _ value = do
          case value of
            Nothing ->
              pure $ ResultNullable Nothing

            Just val ->
              ResultNullable <$> Just <$> output t node.selectionSet execCtx (pure val)

        serialize _ _ _ _ =
          pure $ ResultError "Obtained non FieldNode for field serialisation."

        typeIntrospection _ = introspect t

-- | Create a new field for an object type that is optional (i.e. it can be null) and returns a
-- | list. The resolver must now return a `Maybe`.
nullableListField :: forall m f t a.
  MonadError Error m =>
  Traversable f =>
  OutputType m t =>
  String ->
  t a ->
  Field m (Maybe (f a)) () ()
nullableListField name t =
  Field
    { name
    , description: Nothing
    , args: {}
    , serialize
    , argumentIntrospections: []
    , typeIntrospection }
      where
        serialize (AST.FieldNode node) variables _ values = do
          case values of
            Nothing ->
              pure $ ResultNullable Nothing

            Just vals ->
              ResultNullable <$>
              Just <$>
              ResultList <$>
              fromFoldable <$>
              traverse (output t node.selectionSet variables) (pure <$> vals)
        serialize _ _ _ _ =
          pure $ ResultError "Obtained non FieldNode for field serialisation."

        typeIntrospection _ =
          IntrospectionTypes.TypeIntrospection
            { kind: IntrospectionTypes.List
            , name: Nothing
            , description: Nothing
            , fields: Nothing
            , inputs: Nothing
            , enumValues: Nothing
            , ofType: Just $ \_ -> introspect t
            , possibleTypes: Nothing
            }

-- | Create a tuple with a given name and a plain argument of the given type. Arguments are passed
-- | into the resolver. Arguments created with this function are required and the query fails if
-- | no value or a `null` value is supplied for this argument. To make an argument optional use
-- | `optionalArg` instead.
-- | The name argument tuple can then be used to be added to a field using the `?>` operator.
-- |
-- | *Example:*
-- | ```purescript
-- | objectType "Query"
-- |   :> field "hello" Scalar.string
-- |     ?> arg Scalar.string (SProxy :: _ "name")
-- |     !> \{ name } parent -> pure $ "Hello " <> name
-- | ```
arg :: forall t a n. InputType t => IsSymbol n => t a -> SProxy n -> Tuple (SProxy n) (Argument a)
arg t name =
    Tuple (SProxy :: _ n) $
      Argument
        { description: Nothing
        , resolveValue: input t
        , typeIntrospection: \_ -> introspect t
        }

-- | Create a tuple with a given name and a plain argument of the given type, that is optional.
-- | If the argument is supplied for the field the value is available in the arguments wrapped in
-- | `Just`. If the argument is not supplied or the supplied value is `null` the arguments field
-- | in the resolver is set to `Nothing`.
-- | The name argument tuple can then be used to be added to a field using the `?>` operator.
-- |
-- | *Example:*
-- | ```purescript
-- | objectType "Query"
-- |   :> field "hello" Scalar.string
-- |     ?> optionalArg Scalar.string (SProxy :: _ "name")
-- |     !> (\{ name } parent -> case name of
-- |          Just n -> pure $ "Hello " <> name
-- |          Nothing -> pure "Hello stranger"
-- |     )
-- | ```
optionalArg ::
  forall t a n.
  InputType t =>
  IsSymbol n =>
  t a ->
  SProxy n ->
  Tuple (SProxy n) (Argument (Maybe a))
optionalArg t name =
    Tuple (SProxy :: _ n) $
      Argument
        { description: Nothing
        , resolveValue
        , typeIntrospection: \_ -> introspect t
        }
    where
      -- I am not really happy with this function and it is a bit of a hack:
      -- We are basically proxying the input function and prevent it from failing if null is
      -- supplied as the variable value. It is easy to remove the Nothing case from the literal
      -- case but hard to remove the JSON null from the possible JSON values. I think this is the
      -- right place to implement this logic but I don't know how to remove this case completely
      -- from the input function.
      resolveValue Nothing execCtx = Right Nothing
      resolveValue (Just AST.NullValueNode) execCtx = Right Nothing
      resolveValue a@(Just (AST.VariableNode { name: AST.NameNode n})) execCtx = do
        variableValue <- note ("Unknown variable \"" <> n.value <> "\".") $
          lookup n.value execCtx.variables
        if Json.isNull variableValue then pure Nothing else Just <$> input t a execCtx
      resolveValue a execCtx = Just <$> input t a execCtx


inputObjectType :: String -> InputObjectType {}
inputObjectType name = InputObjectType \_ ->
  { name
  , description: Nothing
  , fieldIntrospection: []
  , input: \_ _ -> Right {}
  }


inputField :: forall t l a. IsSymbol l => InputType t => t a -> SProxy l -> InputField l a
inputField inputType label = InputField
  { name: label
  , introspection:
      IntrospectionTypes.InputValueIntrospection
        { name: reflectSymbol label
        , description: Nothing
        , defaultValue: Nothing
        , type: \_ -> introspect inputType
        }
  , input: input inputType }

-- * DSL Combinator operators

-- | The describe type class is used to allow adding descriptions to various parts of a GraphQL
-- | schema that can have a description. The `.>` operator can be used with the GraphQL object
-- | type DSL to easily add descriptions in various places.
-- |
-- | *Example:*
-- | ```purescript
-- | objectType "User"
-- |   .> "A user of the product."
-- |   :> field "id" Scalar.id
-- |     .> "A unique identifier for this user."
-- | ```
class Describe a where
  describe :: a -> String -> a

infixl 8 describe as .>

instance describeObjectType :: Describe (ObjectType m a) where
  describe (ObjectType configFn) s =
      ObjectType $ \_ ->
        let
          config = configFn unit
          newIntrospection = wrap $ (unwrap config.introspection) { description = Just s }
        in
          config { introspection = newIntrospection }

instance describeField :: Describe (Field m a argsd argsp) where
  describe (Field config) s = Field (config { description = Just s })

instance describeArgument :: Describe (Argument a) where
  describe (Argument config) s = Argument (config { description = Just s })

instance describeArgumentTuple :: Describe (Tuple s (Argument a)) where
  describe (Tuple p (Argument config)) s = Tuple p (Argument (config { description = Just s }))

instance describeEnumType :: Describe (EnumType a) where
  describe (EnumType config) s = EnumType ( config { description = Just s })

instance describeEnumValue :: Describe (EnumValue a) where
  describe (EnumValue config) s = EnumValue ( config { description = Just s })

instance describeInputObjectType :: Describe (InputObjectType a) where
  describe (InputObjectType configFn) s =
    InputObjectType $ \_ -> (configFn unit) { description = Just s }

instance describeInputField :: Describe (InputField s a) where
  describe (InputField config) s =
    InputField $ config { introspection = i }
      where
        { introspection: (IntrospectionTypes.InputValueIntrospection introspection)} = config
        i = IntrospectionTypes.InputValueIntrospection $ introspection { description = Just s }

-- | The `withField` function is used to add fields to an object type.
-- |
-- | When added to an object type the information about the field's arguments are hidden in the
-- | closure by being converted to the `ExecutableField` type.
withField :: forall m a argsd argsp.
  ArgsDefToArgsParam argsd argsp =>
  MonadError Error m =>
  ObjectType m a ->
  Field m a argsd argsp ->
  ObjectType m a
withField (ObjectType objectConfigFn) fld@(
  Field { name, description, typeIntrospection, argumentIntrospections }
) =
  ObjectType updated
    where
      updated _ =
        let
          objectConfig = objectConfigFn unit
          updatedFields = insert name (makeExecutable fld) objectConfig.fields

          fieldIntrospection = IntrospectionTypes.FieldIntrospection
            { name
            , description
            , args: argumentIntrospections
            , type: typeIntrospection
            , deprecationReason: Nothing
            }

          introspection = unwrap objectConfig.introspection

          updatedIntrospection =
            IntrospectionTypes.TypeIntrospection $
              introspection { fields = map (flip Array.snoc fieldIntrospection) (introspection.fields) }
        in
          objectConfig { fields = updatedFields, introspection = updatedIntrospection }

      makeExecutable :: Field m a argsd argsp -> ExecutableField m a
      makeExecutable (Field { args, serialize: serialize' }) = ExecutableField { execute: execute' }
        where
          execute' :: a -> AST.SelectionNode -> ExecutionContext -> m Result
          execute' val node@(AST.FieldNode { arguments: argumentNodes }) variables =
            case argsFromDefinition argumentNodes variables args of
              Right argumentValues -> serialize' node variables argumentValues val
              Left err -> pure (ResultError err)
          execute' _ _ _ = pure (ResultError "Unexpected non field node field execution...")

-- | Operator for the `withField` function: Add a field to an object type.
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
        ExecutionContext ->
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
      ExecutionContext ->
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
          improveInputError err = "Error when reading argument '" <> reflectSymbol key <> "': " <> err
          resolvedValue = lmap improveInputError $ argConfig.resolveValue valueNode variables
          tail =
            argsFromRows
              (RLProxy :: RLProxy ltargsd)
              (RLProxy :: RLProxy ltargsp)
              nodes
              variables
              tailArgsDef
      in Record.insert key <$> resolvedValue <*> tail

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
  Field $ fieldConfig { args = args', serialize = serialize', argumentIntrospections = intros' }
    where
      args' :: Record argsdnew
      args' = Record.insert proxy argument fieldConfig.args

      serialize' ::
        AST.SelectionNode ->
        ExecutionContext ->
        Record argspnew ->
        a ->
        m Result
      serialize' node variables argsnew val =
        fieldConfig.serialize node variables (Record.delete proxy argsnew) val

      intros' :: Array IntrospectionTypes.InputValueIntrospection
      intros' = Array.snoc fieldConfig.argumentIntrospections
        (
          IntrospectionTypes.InputValueIntrospection
            { name: reflectSymbol proxy
            , description: _.description $ unwrap argument
            , type: _.typeIntrospection $ unwrap argument
            , defaultValue: Nothing
            }
        )

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
-- | _Note: When used multiple times on a field the resolvers are chained._
withResolver :: forall m a b argsd argsp.
  MonadError Error m =>
  Field m a argsd argsp ->
  (Record argsp -> b -> m a) ->
  Field m b argsd argsp
withResolver (Field fieldConfig) resolver =
  Field $ fieldConfig { serialize = serialize }
    where
      serialize :: AST.SelectionNode -> ExecutionContext -> Record argsp -> b -> m Result
      serialize node variables args = resolver args >=> fieldConfig.serialize node variables args

-- | Add a resolver to a field that receives the arguments in a record and the parent value.
-- |
-- | *Example:*
-- | ```purescript
-- | objectType "User"
-- |   :> field "friends" Scalar.string
-- |     !> (\_ user -> user >>= loadUserFriends)
-- | ```
infixl 6 withResolver as !>

-- | Add a resolver to a field that does not take the arguments. This is useful in the very common
-- | case that your field has no arguments.
withSimpleResolver :: forall m a b.
  MonadError Error m =>
  Field m a () () ->
  (b -> m a) ->
  Field m b () ()
withSimpleResolver fld resolver = withResolver fld (const resolver)

-- | Add a resolver to a field that receives only the parent value and ignores the arguments.
-- |
-- | *Example:*
-- | ```purescript
-- | objectType "User"
-- |   :> field "friends" Scalar.string
-- |     !!> (\user -> user >>= loadUserFriends)
-- | ```
infixl 6 withSimpleResolver as !!>

-- | Add a resolver that simply maps over the parent value. This is useful when you have a pure
-- | resolver that does not run effects.
withMappingResolver :: forall m a b.
  MonadError Error m =>
  Field m a () () ->
  (b -> a) ->
  Field m b () ()
withMappingResolver fld resolver = withSimpleResolver fld (resolver >>> pure)

-- | Add a pure resolver to a field that simply maps the parent value.
-- |
-- | *Example:*
-- | ```purescript
-- | objectType "User"
-- |   :> field "id" Scalar.string
-- |     !#> _.id
-- | ```
infixl 6 withMappingResolver as !#>

-- | Add a field to an input object type.
-- | Usually this functions operator alias
withInputField :: forall l a r1 r2.
  IsSymbol l
  => Lacks l r1
  => Cons l a r1 r2
  => InputObjectType { | r1 }
  -> InputField l a
  -> InputObjectType { | r2 }
withInputField (InputObjectType objConfig) (InputField inputConfig) =InputObjectType (\_ ->
    let
      config = (objConfig unit)
      fieldLabel = (SProxy :: SProxy l)
      fieldName = reflectSymbol fieldLabel

      input :: Maybe AST.ValueNode -> ExecutionContext -> Either String (Record r2)
      input variableNode execCtx = do
        let readField = Json.caseJsonObject (Right Nothing) (flip Json.getFieldOptional' fieldName)
        recValue <- config.input variableNode execCtx
        case variableNode of
          (Just (AST.VariableNode { name: AST.NameNode n })) ->
            case lookup n.value execCtx.variables of
              Just json -> do
                maybeValue <- readField json
                let hackedVariables = insert "variable_hack" (fromMaybe Json.jsonNull maybeValue) empty
                let hackedCtx = { fragments: execCtx.fragments, variables: hackedVariables }
                fieldValue <-
                  inputConfig.input
                    (Just (AST.VariableNode { name: (AST.NameNode { value: "variable_hack" }) }))
                    hackedCtx
                pure $ Record.insert fieldLabel fieldValue recValue
              Nothing -> Left ("Unknown variable \"" <> n.value <> "\".")

          (Just (AST.ObjectValueNode { fields })) ->
            case find (\(AST.ObjectFieldNode { name: AST.NameNode n }) -> n.value == fieldName) fields of
              Just (AST.ObjectFieldNode { value }) -> do
                fieldValue <- inputConfig.input (Just value) execCtx
                pure $ Record.insert fieldLabel fieldValue recValue
              Nothing -> Left ("Field '" <> fieldName <> "' is required in object type.")

          _ -> Left "Must provide value for required input object value."

      fieldIntrospection = Array.cons inputConfig.introspection config.fieldIntrospection

    in config { input = input, fieldIntrospection = fieldIntrospection }
  )

-- | Add a field to an input object type.
-- |
-- | *Example:*
-- | ```purescript
-- | inputObjectType "User"
-- |   :?> inputField "name" Scalar.string
-- |     .> "The name of the user"
-- | ```
infixl 6 withInputField as :?>

-- | Take a bounded enum and infer an enum type from that value using it's show instance to
-- | represent the enum string. The show function cannot return values that are not valid GraphQL
-- | indentifier strings. For practical reasons this is not checked at compile time. Please refer to
-- | the tutorial for best practices around enum values.
enumType :: forall a. Bounded a => Enum a => Show a => String -> EnumType a
enumType name = EnumType { name, description: Nothing, values }
  where
    values = enumFromTo top bottom <#> \value ->
      EnumValue { name: show value, description: Nothing, value, isValue: eq value }
