module Test.GraphQL.Language.Parser.Operation where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import GraphQL.Language.AST as AST
import GraphQL.Language.Parser (document)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import StringParser (runParser)

type OperationConfig =
  { name :: Maybe AST.NameNode
  , operation :: AST.OperationTypeNode
  , selectionSet :: AST.SelectionSetNode
  , variableDefinitions :: List AST.VariableDefinitionNode
  }

-- | Little helper function that lets us update some keys of the document before
-- | creating it. This way we can easily just edit one or two properties using the
-- | object update notation `_ { name = NameNode { value: "MyQuery" }`
makeDocument :: (OperationConfig -> OperationConfig) -> AST.DocumentNode
makeDocument update =
  let def =
        { name: Nothing
        , operation: AST.Query
        , selectionSet: AST.SelectionSetNode
          { selections: AST.FieldNode
            { name: AST.NameNode { value: "hello" }
            , alias: Nothing
            , selectionSet: Nothing
            , arguments: Nil
            , directives: Nil
            } : Nil
          }
        , variableDefinitions: Nil
        }
  in AST.DocumentNode { definitions: AST.OperationDefinitionNode (update def) : Nil }

parserOperationSpec :: Spec Unit
parserOperationSpec =
  describe "Operation" do
    it "parses a query without query keyword" do
      let result = runParser document """{ hello }"""
      result `shouldEqual` Right (makeDocument identity)

    it "parses a query without query keyword surrounded by whitespace" do
      let result = runParser document "\n  \n\t{ hello }  \n"
      result `shouldEqual` Right (makeDocument identity)

    it "parses an anonymous query with query keyword" do
      let result = runParser document """query { hello }"""
      result `shouldEqual` Right (makeDocument identity)

    it "parses an anonymous mutation" do
      let result = runParser document """mutation { hello }"""
      result `shouldEqual` Right (makeDocument (_ { operation = AST.Mutation }))

    it "parses an anonymous mutation surrounded by whitespace" do
      let result = runParser document "  mutation { hello }\n"
      result `shouldEqual` Right (makeDocument (_ { operation = AST.Mutation }))

    it "parses a named query with query keyword" do
      let result = runParser document """query MyQuery { hello }"""
      result `shouldEqual` Right
        (makeDocument (_ { name = Just $ AST.NameNode { value: "MyQuery" } }))

    it "parses a named query with query keyword surrounded by whitespace" do
      let result = runParser document "  \nquery MyQuery { hello } "
      result `shouldEqual` Right
        (makeDocument (_ { name = Just $ AST.NameNode { value: "MyQuery" } }))

    it "parses a named mutation" do
      let result = runParser document """mutation MyMutation { hello }"""
      result `shouldEqual` Right
        (makeDocument
          (_ { operation = AST.Mutation, name = Just $ AST.NameNode { value: "MyMutation" } }))

    let idVariable =
          AST.VariableDefinitionNode
            { variable: AST.NameNode { value: "id" }
            , defaultValue: Nothing
            , "type": AST.NamedTypeNode
                { "type": AST.SimpleNamedTypeNode { name: AST.NameNode { value: "ID" } } }
            , directives: Nil
            }

    it "parses an anonymous query with a variable" do
      let result = runParser document """query ($id: ID) { hello }"""
      result `shouldEqual` Right (makeDocument (_ { variableDefinitions = idVariable : Nil }))

    it "parses a named query with a variable" do
      let result = runParser document """query MyQuery ($id: ID) { hello }"""
      result `shouldEqual` Right
        (makeDocument
          (_ { variableDefinitions = idVariable : Nil
             , name = Just $ AST.NameNode { value: "MyQuery" } }))

    it "parses a query with multiple variables" do
      let xVariable =
            AST.VariableDefinitionNode
              { variable: AST.NameNode { value: "x" }
              , defaultValue: Nothing
              , "type": AST.NamedTypeNode
                  { "type": AST.SimpleNamedTypeNode { name: AST.NameNode { value: "String" } } }
              , directives: Nil
              }
      let result = runParser document """query ($id: ID, $x: String) { hello }"""
      result `shouldEqual` Right
        (makeDocument (_ { variableDefinitions = idVariable : xVariable : Nil }))
