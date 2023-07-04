module GraphQL.DSL where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Enum (class Enum, enumFromTo)
import Data.List (List, fromFoldable, find)
import Data.Map (empty, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect.Exception (Error, error)
import GraphQL.Execution.Result (Result(..))
import GraphQL.Language.AST as AST
import GraphQL.Type.Class (class InputType, class OutputType, ExecutionContext, input, introspect, output)
import GraphQL.Type.EnumType (EnumType(..), EnumValue(..))
import GraphQL.Type.InputObjectType (InputField(..), InputObjectType(..))
import GraphQL.Type.Introspection.Datatypes as IntrospectionTypes
import GraphQL.Type.ObjectType (Argument(..), ExecutableField(..), Field(..), ObjectType(..))
import GraphQL.Type.UnionType (UnionType(..), filterSelectionSet)
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Row as Row
import Type.RowList as RL
import Unsafe.Coerce (unsafeCoerce)


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
    introspection = IntrospectionTypes.ObjectTypeIntrospection
      { name, description: Nothing, fields: [] }
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
          IntrospectionTypes.NonNullTypeIntrospection { ofType: \_ -> introspect t }


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
          IntrospectionTypes.NonNullTypeIntrospection
            { ofType: \_ ->
                IntrospectionTypes.ListTypeIntrospection { ofType: \_ -> introspect t }
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
          IntrospectionTypes.ListTypeIntrospection { ofType: \_ -> introspect t }


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
-- |     ?> arg Scalar.string (Proxy :: _ "name")
-- |     !> \{ name } parent -> pure $ "Hello " <> name
-- | ```
arg :: forall t a n. InputType t => IsSymbol n => t a -> Proxy n -> Tuple (Proxy n) (Argument a)
arg t _name =
    Tuple (Proxy :: _ n) $
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
-- |     ?> optionalArg Scalar.string (Proxy :: _ "name")
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
  Proxy n ->
  Tuple (Proxy n) (Argument (Maybe a))
optionalArg t _name =
    Tuple (Proxy :: _ n) $
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
      resolveValue Nothing _execCtx = Right Nothing
      resolveValue (Just AST.NullValueNode) _execCtx = Right Nothing
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


inputField :: forall t l a. IsSymbol l => InputType t => t a -> Proxy l -> InputField l a
inputField inputType label = InputField
  { name: label
  , required: true
  , introspection:
      IntrospectionTypes.InputValueIntrospection
        { name: reflectSymbol label
        , description: Nothing
        , defaultValue: Nothing
        , type: \_ -> IntrospectionTypes.NonNullTypeIntrospection
          { ofType: \_ -> introspect inputType
          }
        }
  , input: input inputType
  }


optionalInputField :: forall t l a.
  IsSymbol l =>
  InputType t =>
  t a ->
  Proxy l ->
  InputField l (Maybe a)
optionalInputField inputType label = InputField
  { name: label
  , required: false
  , introspection:
      IntrospectionTypes.InputValueIntrospection
        { name: reflectSymbol label
        , description: Nothing
        , defaultValue: Nothing
        , type: \_ -> introspect inputType
        }
  , input: \maybeValNode execContext ->
    case maybeValNode of
      Nothing -> Right Nothing
      Just AST.NullValueNode -> Right Nothing
      Just _ -> Just <$> input inputType maybeValNode execContext
  }


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
          updateDescription (IntrospectionTypes.ObjectTypeIntrospection introspection) =
            IntrospectionTypes.ObjectTypeIntrospection $
              introspection { description = Just s }
          updateDescription other = other
        in
          config { introspection = updateDescription config.introspection }


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

          updateIntrospection (IntrospectionTypes.ObjectTypeIntrospection introspection) =
            IntrospectionTypes.ObjectTypeIntrospection $
              introspection { fields = Array.snoc introspection.fields fieldIntrospection }
          updateIntrospection other = other

          updatedIntrospection = updateIntrospection objectConfig.introspection
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
class ArgsDefToArgsParam (argsd :: Row Type) (argsp :: Row Type)
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
    argsFromDefinition = argsFromRows (Proxy :: Proxy largsd) (Proxy :: Proxy largsp)


-- | ArgsDefToArgsParam that works on row lists for matching implementations
class ArgsFromRows
  (largsd :: RL.RowList Type)
  (largsp :: RL.RowList Type)
  (argsd :: Row Type)
  (argsp :: Row Type)
  where
    argsFromRows ::
      Proxy largsd ->
      Proxy largsp ->
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
    argsFromRows _argsdProxy _argspProxy nodes variables argsd =
      let key = Proxy :: Proxy l
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
              (Proxy :: Proxy ltargsd)
              (Proxy :: Proxy ltargsp)
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
  Tuple (Proxy n) (Argument arg) ->
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
-- |     ?> arg (Proxy :: Proxy "name") Scalar.string
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
  => Row.Lacks l r1
  => Row.Cons l a r1 r2
  => InputObjectType { | r1 }
  -> InputField l a
  -> InputObjectType { | r2 }
withInputField (InputObjectType objConfig) (InputField inputConfig) = InputObjectType (\_ ->
    let
      config = (objConfig unit)
      fieldLabel = (Proxy :: Proxy l)
      fieldName = reflectSymbol fieldLabel

      -- This function calls the old input function recursively and then also reads the new field
      -- from the provided object
      input :: Maybe AST.ValueNode -> ExecutionContext -> Either String (Record r2)
      input valueNode execCtx = do
        recValue <- config.input valueNode execCtx
        -- There are two possibilities to pass in input values: Inline or as a variable
        case valueNode of
          -- if it is a variable, we have to get the fields from JSON
          (Just (AST.VariableNode { name: AST.NameNode n })) ->
            case lookup n.value execCtx.variables of
              Just json -> do
                maybeValue <- lmap Json.printJsonDecodeError $
                  -- TODO: We should probably throw if the variable is not an object...
                  Json.caseJsonObject (Right Nothing) (_ `Json.getFieldOptional'` fieldName) json
                let hackedVariables = insert "variable_hack" (fromMaybe Json.jsonNull maybeValue) empty
                let hackedCtx = { fragments: execCtx.fragments, variables: hackedVariables }
                fieldValue <-
                  inputConfig.input
                    (Just (AST.VariableNode { name: (AST.NameNode { value: "variable_hack" }) }))
                    hackedCtx
                pure $ Record.insert fieldLabel fieldValue recValue
              Nothing -> Left ("Unknown variable \"" <> n.value <> "\".")

          -- else we have to get the values from the node
          (Just (AST.ObjectValueNode { fields })) ->
            -- here we can simply search through all the fields and try to find our value
            case find (\(AST.ObjectFieldNode { name: AST.NameNode n }) -> n.value == fieldName) fields of
              Just (AST.ObjectFieldNode { value }) -> do
                fieldValue <- inputConfig.input (Just value) execCtx
                pure $ Record.insert fieldLabel fieldValue recValue
              Nothing ->
                if inputConfig.required
                then Left ("Field '" <> fieldName <> "' is required in object type.")
                else do
                  fieldValue <- inputConfig.input Nothing execCtx
                  pure $ Record.insert fieldLabel fieldValue recValue

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


class UnionDefinition (defRow :: Row Type) (varRow :: Row Type) (ctx :: Type -> Type)
    | defRow -> varRow
    , varRow -> defRow where
      union ::
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
    union name defRecord =
      let
        typeIntrospections = unionIntrospection (Proxy :: Proxy defRowList) defRecord
        output selectionSetNode execCtx ctxValue = ctxValue >>=
          unionResolver
            (Proxy :: Proxy defRowList)
            (Proxy :: Proxy varRowList)
            defRecord
            selectionSetNode
            execCtx
      in
        UnionType $ \_ -> { name, description: Nothing, typeIntrospections, output }


class UnionIntrospection
  (defRowList :: RL.RowList Type)
  (defRow :: Row Type)
  where
    unionIntrospection ::
      Proxy defRowList ->
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
          unionIntrospection _ defRecord =
            let
              ObjectType configFn = Record.get (Proxy :: Proxy l) defRecord
              config = configFn unit
            in
              -- Recursively call function
              Array.cons config.introspection $
                unionIntrospection
                  (Proxy :: Proxy defRowListTail)
                  -- We will just read from this value
                  (unsafeCoerce defRecord :: Record defRowTail)

else instance unionIntrospectionNil :: UnionIntrospection RL.Nil defRow where
  unionIntrospection _ _ = []


class UnionResolver
  (defRowList :: RL.RowList Type)
  (varRowList :: RL.RowList Type)
  (defRow :: Row Type)
  (varRow :: Row Type)
  (ctx :: Type -> Type)
  where
    unionResolver ::
      Proxy defRowList ->
      Proxy varRowList ->
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
            Proxy (RL.Cons l (ObjectType ctx a) defRowListTail) ->
            Proxy (RL.Cons l a varRowListTail) ->
            Record defRow ->
            Maybe AST.SelectionSetNode ->
            ExecutionContext ->
            (Variant.Variant varRow) ->
            ctx Result
          unionResolver _defProxy _varProxy _defRecord Nothing _execCtx =
            const $ throwError $ error "Missing selection set for union type"
          unionResolver _defProxy _varProxy defRecord (Just selection) execCtx =
            let
              objectType@(ObjectType config) = Record.get (Proxy :: Proxy l) defRecord
              typename = (config unit).name
              filteredSelection = filterSelectionSet typename selection execCtx
              recResolver ::
                (Variant.Variant varRowTail) ->
                ctx Result
              recResolver =
                unionResolver
                  (Proxy :: Proxy defRowListTail)
                  (Proxy :: Proxy varRowListTail)
                  (unsafeCoerce defRecord :: Record defRowTail)
                  (Just selection)
                  execCtx
            in
                recResolver
                  # Variant.on (Proxy :: Proxy l)
                    (pure >>> output objectType (Just filteredSelection) execCtx)

else instance unionResolverNil :: Functor ctx => UnionResolver RL.Nil RL.Nil defRow varRow ctx where
  unionResolver _ _ _ _ _ = unsafeCoerce >>> Variant.case_

