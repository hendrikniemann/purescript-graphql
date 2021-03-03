module GraphQL.Language.Printer where

import Prelude

import Data.Foldable (intercalate)
import Data.String (Pattern(..), split)
import GraphQL.Language.AST as AST

data Doc
  = Nil
  | Text String Doc
  | Line Int Doc

instance semigroupDoc :: Semigroup Doc where
  append Nil d = d
  append d Nil = d
  append (Text s1 Nil) (Text s2 ds) = Text (s1 <> s2) ds
  append (Text s Nil) d = Text s d
  append (Text s ds) d = Text s (append ds d)
  append (Line i Nil) d = Line i d
  append (Line i ds) d = Line i (append ds d)

instance monoidDoc :: Monoid Doc where
  mempty = Nil

text :: String -> Doc
text str = Text str Nil

line :: Doc
line = Line 0 Nil 

nest :: Int -> Doc -> Doc
nest indent Nil = Nil
nest indent (Text s ds) = Text s (nest indent ds)
nest indent (Line i ds) = Line (i + indent) (nest indent ds)

layout :: Doc -> String
layout Nil = ""
layout (Text s ds) = s <> (layout ds)
layout (Line i ds) = "\n" <> repeat " " i <> layout ds

repeat :: String -> Int -> String
repeat str times = repeatRec str times ""
  where
    repeatRec _ 0 acc = acc
    repeatRec s i acc = repeatRec s (i - 1) (s <> acc)

printNameNode :: AST.NameNode -> Doc
printNameNode (AST.NameNode { value }) = text value

printSimpleNamedTypeNode :: AST.SimpleNamedTypeNode -> Doc
printSimpleNamedTypeNode (AST.SimpleNamedTypeNode { name }) = printNameNode name

printTypeNode :: AST.TypeNode -> Doc
printTypeNode (AST.NamedTypeNode { "type": t }) = printSimpleNamedTypeNode t
printTypeNode (AST.NonNullTypeNode { "type": t }) = printTypeNode t <> text "!"
printTypeNode (AST.ListTypeNode { "type": t }) = text "[" <> printTypeNode t <> text "]"

printObjectFieldNode :: AST.ObjectFieldNode -> Doc
printObjectFieldNode (AST.ObjectFieldNode { name, value }) =
  printNameNode name <> text ": " <> printValueNode value

printValueNode :: AST.ValueNode -> Doc
printValueNode (AST.VariableNode { name }) = text "$" <> printNameNode name
printValueNode (AST.IntValueNode { value }) = text value
printValueNode (AST.FloatValueNode { value }) = text value
printValueNode (AST.StringValueNode { value, block }) =
  if block then printBlockString value else text ("\"" <> value <> "\"")
printValueNode (AST.BooleanValueNode { value }) = if value then text "true" else text "false"
printValueNode AST.NullValueNode = text "null"
printValueNode (AST.EnumValueNode { name }) = printNameNode name
printValueNode (AST.ListValueNode { values }) =
  text "[" <> intercalate (text ", ") (map printValueNode values) <> text "]" 
printValueNode (AST.ObjectValueNode { fields }) =
  text "{" <> intercalate (text ", ") (map printObjectFieldNode fields) <> text "}" 

printBlockString :: String -> Doc
printBlockString str =
  let lines = map text (split (Pattern "\n") str)
  in text "\"\"\"" <> line <> intercalate line lines <> line <> text "\"\"\""
