module GraphQL.Language.AST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data DocumentNode
  = DocumentNode { definitions :: List DefinitionNode }

data DefinitionNode
  = OperationDefinitionNode
    { operation :: OperationTypeNode
    , name :: Maybe NameNode
    , variableDefinitions :: List VariableDefinitionNode
    , selectionSet :: SelectionSetNode
    }
  | FragmentDefinitionNode
    { name :: NameNode
    , typeCondition :: SimpleNamedTypeNode
    , directives :: List DirectiveNode
    , selectionSet :: SelectionSetNode
    }

data NameNode
  = NameNode { value :: String }

data OperationTypeNode
  = Query
  | Mutation
  | Subscription

data VariableDefinitionNode
  = VariableDefinitionNode
    { variable :: NameNode
    , type :: TypeNode
    , defaultValue :: Maybe ValueNode
    , directives :: List DirectiveNode
    }

data SelectionSetNode
  = SelectionSetNode { selections :: List SelectionNode }

data SelectionNode
  = FieldNode
    { alias :: Maybe NameNode
    , name :: NameNode
    , arguments :: List ArgumentNode
    , directives :: List DirectiveNode
    , selectionSet :: Maybe SelectionSetNode
    }
  | FragmentSpreadNode
    { name :: NameNode
    , directives :: List DirectiveNode
    }
  | InlineFragmentNode
    { typeCondition :: SimpleNamedTypeNode
    , directives :: List DirectiveNode
    , selectionSet :: SelectionSetNode
    }

data ArgumentNode
  = ArgumentNode
    { name :: NameNode
    , value :: ValueNode
    }

data ValueNode
  = VariableNode { name :: NameNode }
  | IntValueNode { value :: String }
  | FloatValueNode { value :: String }
  | StringValueNode { value :: String, block :: Boolean }
  | BooleanValueNode { value :: Boolean }
  | NullValueNode
  | EnumValueNode { name :: NameNode }
  | ListValueNode { values :: List ValueNode }
  | ObjectValueNode { fields :: List ObjectFieldNode }

data ObjectFieldNode
  = ObjectFieldNode
    { name :: NameNode
    , value :: ValueNode
    }

data DirectiveNode
  = DirectiveNode
    { name :: NameNode
    , arguments :: List ArgumentNode
    }

data TypeNode
  = NamedTypeNode {type :: SimpleNamedTypeNode}
  | ListTypeNode {type :: TypeNode}
  | NonNullTypeNode {type :: TypeNode} -- This is wrong, see original definition

data SimpleNamedTypeNode
  = SimpleNamedTypeNode {name :: NameNode}

derive instance genericDocumentNode :: Generic DocumentNode _

derive instance eqDocumentNode :: Eq DocumentNode

instance showDocumentNode :: Show DocumentNode where
  show = genericShow

derive instance genericDefinitionNode :: Generic DefinitionNode _

instance showDefinitionNode :: Show DefinitionNode where
  show = genericShow

derive instance eqDefinitionNode :: Eq DefinitionNode

derive instance genericNameNode :: Generic NameNode _

instance showNameNode :: Show NameNode where
  show = genericShow

derive instance eqNameNode :: Eq NameNode

derive instance genericOperationTypeNode :: Generic OperationTypeNode _

instance showOperationTypeNode :: Show OperationTypeNode where
  show = genericShow

derive instance eqOperationTypeNode :: Eq OperationTypeNode

derive instance genericSimpleNamedTypeNode :: Generic SimpleNamedTypeNode _

instance showSimpleNamedTypeNode :: Show SimpleNamedTypeNode where
  show = genericShow

derive instance eqSimpleNamedTypeNode :: Eq SimpleNamedTypeNode

derive instance genericDirectiveNode :: Generic DirectiveNode _

instance showDirectiveNode :: Show DirectiveNode where
  show = genericShow

derive instance eqDirectiveNode :: Eq DirectiveNode

derive instance genericVariableDefinitionNode :: Generic VariableDefinitionNode _

instance showVariableDefinitionNode :: Show VariableDefinitionNode where
  show = genericShow

derive instance eqVariableDefinitionNode :: Eq VariableDefinitionNode

derive instance genericArgumentNode :: Generic ArgumentNode _

derive instance eqArgumentNode :: Eq ArgumentNode

derive instance genericTypeNode :: Generic TypeNode _

derive instance eqTypeNode :: Eq TypeNode

derive instance genericObjectFieldNode :: Generic ObjectFieldNode _

derive instance eqObjectFieldNode :: Eq ObjectFieldNode

derive instance genericSelectionSetNode :: Generic SelectionSetNode _

derive instance eqSelectionSetNode :: Eq SelectionSetNode

derive instance genericSelectionNode :: Generic SelectionNode _

instance showSelectionNode :: Show SelectionNode where
  show x = genericShow x

derive instance eqSelectionNode :: Eq SelectionNode

derive instance genericValueNode :: Generic ValueNode _

instance showValueNode :: Show ValueNode where
  show x = genericShow x

derive instance eqValueNode :: Eq ValueNode

instance showArgumentNode :: Show ArgumentNode where
  show = genericShow

instance showTypeNode :: Show TypeNode where
  show x = genericShow x

instance showObjectFieldNode :: Show ObjectFieldNode where
  show x = genericShow x

instance showSelectionSetNode :: Show SelectionSetNode where
  show x = genericShow x
 {-
data TypeSystemDefinitionNode
  = SchemaDefinitionNode
    { directives :: List DirectiveNode
    , operationTypes :: List OperationTypeDefinitionNode
    }
  | ScalarTypeDefinitionNode
    { description :: Maybe StringValueNode
    , name :: NameNode
    , directives :: List DirectiveNode
    }
  | ObjectTypeDefinitionNode
    { description :: Maybe StringValueNode
    , name :: NameNode
    , interfaces :: List NamedTypeNode
    , directives :: List DirectiveNode
    , fields :: List FieldDefinitionNode
    }
  | InterfaceTypeDefinitionNode
    { description :: Maybe StringValueNode
    , name :: NameNode
    , directives :: List DirectiveNode
    , fields :: List FieldDefinitionNode
    }
  | UnionTypeDefinitionNode
    { description :: Maybe StringValueNode
    , name :: NameNode
    , directives :: List DirectiveNode
    , types :: List NamedTypeNode
    }
  | EnumTypeDefinitionNode
    { description :: Maybe StringValueNode
    , name :: NameNode
    , directives :: List DirectiveNode
    , values :: List EnumValueDefinitionNode
    }
    { description :: Maybe StringValueNode
    , name :: NameNode
    , directives :: List DirectiveNode
    , fields :: List InputValueDefinitionNode
    }
  | DirectiveDefinitionNode
    { description :: Maybe StringValueNode
    , name :: NameNode
    , arguments :: List InputValueDefinitionNode
    , locations :: List NameNode
    }

data OperationTypeDefinitionNode = OperationTypeDefinitionNode
  { operation :: OperationTypeNode
  , type :: NamedTypeNode
  }


data FieldDefinitionNode = FieldDefinitionNode
  { description :: Maybe StringValueNode
  , name :: NameNode
  , arguments :: List InputValueDefinitionNode
  , type :: TypeNode
  , directives :: List DirectiveNode
  }

data InputValueDefinitionNode = InputValueDefinitionNode
  { description :: Maybe StringValueNode
  , name :: NameNode
  , type :: TypeNode
  , defaultValue :: Maybe ValueNode
  , directives :: List DirectiveNode
  }

data EnumValueDefinitionNode = EnumValueDefinitionNode
  { description :: Maybe StringValueNode
  , name :: NameNode
  , directives :: List DirectiveNode
  }

data TypeSystemExtensionNode
  = SchemaExtensionNode
    { directives :: List DirectiveNode
    , operationTypes :: List OperationTypeDefinitionNode
    }
  | ScalarTypeExtensionNode
    { name :: NameNode
    , directives :: List DirectiveNode
    }
  | ObjectTypeExtensionNode
    { name :: NameNode
    , interfaces :: List NamedTypeNode
    , directives :: List DirectiveNode
    , fields :: List FieldDefinitionNode
    }
  | InterfaceTypeExtensionNode
    { name :: NameNode
    , directives :: List DirectiveNode
    , fields :: List FieldDefinitionNode
    }
  | UnionTypeExtensionNode
    { name :: NameNode
    , directives :: List DirectiveNode
    , types :: List NamedTypeNode
    }
  | EnumTypeExtensionNode
    { name :: NameNode
    , directives :: List DirectiveNode
    , values :: List EnumValueDefinitionNode
    }
  | InputObjectTypeExtensionNode
    { name :: NameNode
    , directives :: List DirectiveNode
    , fields :: List InputValueDefinitionNode
    }
-}