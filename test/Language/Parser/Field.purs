module Test.Language.Parser.Field where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import GraphQL.Language.AST (ArgumentNode(..), DirectiveNode, NameNode(..), SelectionNode(..), SelectionSetNode, ValueNode(..))
import GraphQL.Language.Parser (selection)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (runParser)

baseField ::
  { alias :: Maybe NameNode
  , arguments :: List ArgumentNode
  , directives :: List DirectiveNode
  , name :: NameNode
  , selectionSet :: Maybe SelectionSetNode
  }
baseField =
  { alias: Nothing
  , name: NameNode { value: "hello" }
  , arguments: Nil
  , directives: Nil
  , selectionSet: Nothing
  }

fieldSpec :: Spec Unit
fieldSpec =
  describe "selection parser" do
    it "parses a simple field selection" $
      runParser selection "  hello " `shouldEqual` Right (FieldNode baseField)
    it "parses a field with an alias" $
      runParser selection "  alias: hello "
        `shouldEqual`
          Right (FieldNode (baseField { alias = Just (NameNode { value: "alias" })}))
    it "parses a field with arguments" do
      let args =
            (Cons
              (ArgumentNode { name: NameNode {value: "arg1"}, value: IntValueNode {value: "1"}})
              (Cons
                (ArgumentNode { name: NameNode {value: "arg2"}, value: NullValueNode })
                Nil
              )
            )
      runParser selection "hello(arg1: 1, arg2: null)"
        `shouldEqual`
          Right (FieldNode (baseField { arguments = args }))