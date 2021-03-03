-- | This module is a little bit of a hack and it might be replaced in the future by a real
-- | transformation from an AST to a normalized AST. Here we will keep the data structures exported
-- | from the AST module and replace fragments by inlining them with their selection set.
module GraphQL.Execution.FlattenAst (flattenAst) where

import Prelude

import Data.Foldable (find, foldl)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import GraphQL.Language.AST as AST
import GraphQL.Language.AST.Util (nameFromNameNode, selectionsFromSelectionSet)


flattenAst :: AST.DocumentNode -> AST.DocumentNode
flattenAst (AST.DocumentNode doc) = AST.DocumentNode $ doc { definitions = defs }
  where
    fragments = doc.definitions `flip List.mapMaybe` case _ of
      (AST.FragmentDefinitionNode config) -> Just config
      _ -> Nothing

    fragmentsMap :: Map.Map String AST.SelectionSetNode
    fragmentsMap =
      foldl (\m c -> Map.insert (nameFromNameNode c.name) c.selectionSet m) Map.empty fragments

    findFragment fragmentName = find (_.name >>> (_ == fragmentName)) fragments

    replaceFragment :: AST.SelectionNode -> List.List AST.SelectionNode
    replaceFragment f@(AST.FieldNode _) = List.singleton f
    replaceFragment f@(AST.FragmentSpreadNode { name }) =
      fromMaybe (List.singleton f) $
      selectionsFromSelectionSet <$>
      flattenSelectionSet <$>
      _.selectionSet <$>
      findFragment name
    replaceFragment (AST.InlineFragmentNode { selectionSet }) =
      selectionsFromSelectionSet $ flattenSelectionSet selectionSet

    flattenSelectionSet :: AST.SelectionSetNode -> AST.SelectionSetNode
    flattenSelectionSet (AST.SelectionSetNode set) = AST.SelectionSetNode { selections }
      where
        selections = set.selections >>= replaceFragment

    defs = doc.definitions `flip List.mapMaybe` case _ of
      (AST.OperationDefinitionNode def) ->
        Just $ AST.OperationDefinitionNode $
          def { selectionSet = flattenSelectionSet def.selectionSet }

      _ -> Nothing