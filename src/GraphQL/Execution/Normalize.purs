-- | This module is used to normalize the query AST into an Executable Syntax Tree
module GraphQL.Execution.Normalize where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), filter, find, mapMaybe)
import Data.Maybe (Maybe(..))
import GraphQL.Language.AST as AST

newtype NormalizedOperationNode =
  NormalizedOperationNode {  }

newtype NormalizedSelectionSetNode =
  NormalizedSelectionSetNode { selections :: List NormalizedSelectionNode }

data NormalizedSelectionNode
  = NormalizedFieldSelectionNode {}

normaliseAst :: AST.DocumentNode -> Maybe String -> Either String NormalizedOperationNode
normaliseAst (AST.DocumentNode { definitions }) operationName = Left "TODO"
  where
    -- In GraphQL the document can potentially contain multiple operations.
    -- In this case the request needs to supply a parameter called "operationName" the defines
    -- which of the operations should be executed.
    -- See: https://graphql.github.io/graphql-spec/June2018/#sec-Executing-Requests
    operation = case filter selectedOperationFilter definitions of
      Cons op Nil -> pure op
      Nil -> Left "No operation provided for execution."
      _ -> Left "Multiple operations provided for execution."

    selectedOperationFilter (AST.OperationDefinitionNode config) = case operationName of
      Nothing -> true
      op -> op == map (\(AST.NameNode { value }) -> value) config.name
    selectedOperationFilter _ = false

    -- Collect all operations that are fragments
    fragments = definitions `flip mapMaybe` case _ of
      (AST.FragmentDefinitionNode config) -> Just config
      _ -> Nothing

    findFragment fragmentName = find (_.name >>> (_ == fragmentName)) fragments
