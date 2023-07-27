module GraphQL.Type.ObjectType
  ( ObjectType(..)
  , ExecutableField(..)
  , Field(..)
  , Argument(..)
  ) where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadError, catchError)
import Data.Argonaut as Json
import Data.Either (Either)
import Data.List (List, singleton)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, message)
import GraphQL.Execution.Result (Result(..))
import GraphQL.Language.AST as AST
import GraphQL.OptionallyParallel (class OptionallyParallel, optionallyParallel, optionallySequential)
import GraphQL.Type.Class (class GraphQLType, class OutputType, ExecutionContext)
import GraphQL.Type.Introspection.Datatypes as IntrospectionTypes


newtype ObjectType m a =
  ObjectType (Unit ->
    { name :: String
    , fields :: Map String (ExecutableField m a)
    , introspection :: IntrospectionTypes.TypeIntrospection
    }
  )


derive instance newtypeObjectType :: Newtype (ObjectType m a) _


instance graphqlTypeObjectType :: GraphQLType (ObjectType m) where
  introspect (ObjectType configFn) = (configFn unit).introspection


instance (MonadError Error m, OptionallyParallel f m) => OutputType m (ObjectType m) where
  -- output :: forall t a. t a -> Maybe AST.SelectionSetNode -> ExecutionContext -> m a -> m Result
  output (ObjectType fno) (Just (AST.SelectionSetNode { selections })) execCtx mValue = do
    value <- mValue
    let allFields = selections >>= collectField
    let resolvingFields = serializeField value <$> allFields
    map ResultObject $ (optionallySequential :: f ~> m) $ sequence $ (optionallyParallel :: m ~> f ) <$> resolvingFields
      where
        -- | Flatten the selection node structure by spreading all fragments into the list
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

        -- serializeField :: a -> AST.SelectionNode -> m (Tuple String Result)
        serializeField value node@(AST.FieldNode fld) = do
          let (AST.NameNode { value: name }) = fld.name
          let (AST.NameNode { value: alias }) = fromMaybe fld.name fld.alias
          let o = fno unit
          case lookup name o.fields of
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
        serializeField _ _ = pure $ Tuple "unknown" $ ResultError "Unexpected fragment spread!"

  output _ _ _ _ = pure $ ResultError "Missing subselection on object type."


instance showObjectType :: Show (ObjectType m a) where
  show (ObjectType config) = "type " <> (config unit).name


instance lazyObjectType :: Lazy (ObjectType m a) where
  defer fn = ObjectType $ \_ -> unwrap (fn unit) unit


-- | The executable field loses the information about it's arguments types. This is needed to add it
-- | to the map of arguments of the object type. The execute function will extract the arguments of
-- | the field from the AST.
newtype ExecutableField m a =
  ExecutableField { execute :: a -> AST.SelectionNode -> ExecutionContext -> m Result }


derive instance newtypeExecutableField :: Newtype (ExecutableField m a) _


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


derive instance newtypeField :: Newtype (Field m a argsd argsp) _


-- | Fields can have multiple arguments. Arguments are constructed using the `arg` function from
-- | this module and the `?>` operator.
newtype Argument a =
  Argument
    { description :: Maybe String
    , typeIntrospection :: Unit -> IntrospectionTypes.TypeIntrospection
    , resolveValue :: Maybe AST.ValueNode -> ExecutionContext -> Either String a
    , defaultValue :: Maybe a
    , showDefaultValue :: a -> String
    }


derive instance newtypeArgument :: Newtype (Argument a) _
