module Test.Language.Parser.Value where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.List (List(..), (:))
import GraphQL.Language.AST (NameNode(..), ObjectFieldNode(..), ValueNode(..))
import GraphQL.Language.Parser (value)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (runParser)

valueSpec :: Spec Unit
valueSpec =
  describe "value parser" do
    it "parses a variable" $
      runParser value "$hello" `shouldEqual` Right (VariableNode {name: NameNode {value: "hello"}})

    it "parses a simple string" $
      runParser value "\"some string\"" `shouldEqual` Right (StringValueNode {block: false, value: "some string"})
    
    it "parses integer values" do
      runParser value "1234" `shouldEqual` Right (IntValueNode {value: "1234"})
      runParser value "0" `shouldEqual` Right (IntValueNode {value: "0"})
      runParser value "-9823498" `shouldEqual` Right (IntValueNode {value: "-9823498"})
      runParser value "-0" `shouldEqual` Right (IntValueNode {value: "-0"})
      isLeft (runParser value "02384") `shouldEqual` true
      isLeft (runParser value "-02384") `shouldEqual` true
    
    it "parses float values" do
      runParser value "234.23476" `shouldEqual` Right (FloatValueNode {value: "234.23476"})
      runParser value "-1.2" `shouldEqual` Right (FloatValueNode {value: "-1.2"})
      runParser value "1234E72" `shouldEqual` Right (FloatValueNode {value: "1234E72"})
      runParser value "434E-2" `shouldEqual` Right (FloatValueNode {value: "434E-2"})
      isLeft (runParser value "-023.000") `shouldEqual` true
    
    it "parses boolean values" do
      runParser value "true" `shouldEqual` Right (BooleanValueNode {value: true})
      runParser value "false" `shouldEqual` Right (BooleanValueNode {value: false})
    
    it "parses null values" do
      runParser value "null" `shouldEqual` Right NullValueNode
    
    it "parses enum values" do
      runParser value "SOME_ENUM" `shouldEqual` Right (EnumValueNode {name: NameNode {value: "SOME_ENUM"}})
      runParser value "uglyEnum" `shouldEqual` Right (EnumValueNode {name: NameNode {value: "uglyEnum"}})
      runParser value "lower" `shouldEqual` Right (EnumValueNode {name: NameNode {value: "lower"}})
      runParser value "WITH0" `shouldEqual` Right (EnumValueNode {name: NameNode {value: "WITH0"}})
    
    it "parses list values" do
      let listOf1 = IntValueNode {value: "1"} : Nil
      runParser value "[]" `shouldEqual` Right (ListValueNode {values: Nil})
      runParser value "[1]" `shouldEqual` Right (ListValueNode {values: listOf1})
      runParser value "[ 1   ]" `shouldEqual` Right (ListValueNode {values: listOf1})
      runParser value "[2 1]"
        `shouldEqual`
          Right (ListValueNode {values: IntValueNode {value: "2"} : listOf1})
      runParser value "[2, 1]"
        `shouldEqual`
          Right (ListValueNode {values: IntValueNode {value: "2"} : listOf1})
      runParser value "[\"hello\"]"
        `shouldEqual`
          Right (ListValueNode {values: StringValueNode {block: false, value: "hello"} : Nil})

    it "parses input object values" do
      runParser value "{field: 1}"
        `shouldEqual`
          Right (
            ObjectValueNode
              {fields:
                  ObjectFieldNode { name: NameNode { value: "field" }, value: IntValueNode {value: "1"}} : Nil
              }
          )
      runParser value "{field1: 1.3, field2: null}"
        `shouldEqual`
          Right (
            ObjectValueNode
              { fields:
                  ObjectFieldNode { name: NameNode { value: "field1" }, value: FloatValueNode {value: "1.3" }}
                  : ObjectFieldNode { name: NameNode { value: "field2" }, value: NullValueNode}
                  : Nil
              }
          )