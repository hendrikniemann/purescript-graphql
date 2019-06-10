module Language.TestParser where

import Prelude

import Data.Either (Either(..), isLeft)
import GraphQL.Language.AST (NameNode(..))
import GraphQL.Language.Parser (name)
import Test.Language.Parser.Field (fieldSpec)
import Test.Language.Parser.Value (valueSpec)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (runParser)

testParser :: Spec Unit
testParser =
  describe "Parser Tests" do
    it "parses simple name nodes" do
      runParser name "hello" `shouldEqual` Right (NameNode { value: "hello" })
      runParser name "ABC" `shouldEqual` Right (NameNode { value: "ABC" })
      runParser name "__X1231" `shouldEqual` Right (NameNode { value: "__X1231" })
      isLeft (runParser name "123") `shouldEqual` true
    fieldSpec
    valueSpec
    -- it "parses a simple query string" do
    --   let actual = parse "query { helloWorld }"
    --   let expected = DocumentNode { definitions: Cons query Nil }
    --   (isRight actual) `shouldEqual` true
    --   either throwError (_ `shouldEqual` expected) actual
