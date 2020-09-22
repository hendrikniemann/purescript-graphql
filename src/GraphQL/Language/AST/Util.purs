module GraphQL.Language.AST.Util where

import Data.List (List)
import GraphQL.Language.AST (NameNode(..), SelectionNode, SelectionSetNode(..))

nameFromNameNode :: NameNode -> String
nameFromNameNode (NameNode node) = node.value

selectionsFromSelectionSet :: SelectionSetNode -> List SelectionNode
selectionsFromSelectionSet (SelectionSetNode set) = set.selections
