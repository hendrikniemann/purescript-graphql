module Test.GraphQL.Language.Parser where

import Prelude

import Data.Either (Either(..), isLeft)
import GraphQL.Language.AST (NameNode(..))
import GraphQL.Language.Parser (name)
import Test.GraphQL.Language.Parser.Field (fieldSpec)
import Test.GraphQL.Language.Parser.Operation (parserOperationSpec)
import Test.GraphQL.Language.Parser.Value (valueSpec)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import StringParser (runParser)

parserSpec :: Spec Unit
parserSpec =
  describe "Parser" do
    describe "NameNode" do
      it "parses simple name nodes" do
        runParser name "hello" `shouldEqual` Right (NameNode { value: "hello" })
        runParser name "ABC" `shouldEqual` Right (NameNode { value: "ABC" })
        runParser name "__X1231" `shouldEqual` Right (NameNode { value: "__X1231" })
        isLeft (runParser name "123") `shouldEqual` true
    fieldSpec
    valueSpec
    parserOperationSpec
