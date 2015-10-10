module Language.Swift.Quote.Syntax where

import Data.Text (Text)

data Module = Module Expression
  deriving (Show, Eq)

data Expression
  = Expression1 (Maybe String) PrefixExpression (Maybe [BinaryExpression])
  deriving (Show, Eq)

data PrefixExpression
  = PrefixExpression1 (Maybe String) {- prefixOperator -} PostfixExpression
  | PrefixExpression2 String -- identifier
  deriving (Show, Eq)

data PostfixExpression
  = PostfixExpression1 PrimaryExpression
  | PostfixExpression2 PostfixExpression String -- postfix-operator
  | PostfixExpression3 FunctionCall
  deriving (Show, Eq)

data ExpressionElement = ExpressionElement (Maybe String) Expression
  deriving (Show, Eq)

data FunctionCall = FunctionCall PostfixExpression (Maybe (Maybe [ExpressionElement])) (Maybe Closure)
  deriving (Show, Eq)

data Closure = Closure
  deriving (Show, Eq)

data PrimaryExpression
  = PrimaryExpression1 LiteralExpression
  | PrimaryExpression2 SelfExpression
  deriving (Show, Eq)

data SelfExpression
  = Self1
  | Self2 String -- identifier
  | Self3 [Expression]
  | Self4
  deriving (Show, Eq)

data Literal
  = IntegerLiteral Integer
  | FloatingPointLiteral Double
  | StringLiteral String
  | BooleanLiteral Bool
  | NilLiteral
  deriving (Show, Eq)

data LiteralExpression
  = RegularLiteral Literal
  | SpecialLiteral String
  deriving (Show, Eq)

data BinaryExpression
  = BinaryExpression1
    { beOperator :: String
    , bePrefixExpression :: PrefixExpression
    }
  | BinaryExpression2
    { beTryOperator :: Maybe String
    , bePrefixExpression :: PrefixExpression
    }
  | BinaryExpression3 (Maybe String, Expression) (Maybe String) PrefixExpression
  | BinaryExpression4 String Type
  deriving (Show, Eq)

data Statement
  = ForStatement
    { forInitE :: Maybe ForInit
    , forCondE :: Maybe Expression
    , forNextE :: Maybe Expression
    , forBlock :: CodeBlock
    }
  | ForInStatement
    { fiPattern :: Pattern
    , fiExpression :: Expression
    , fiWhereClause :: Maybe Expression
    , fiBlock :: CodeBlock
    }
  | BranchStatement
  | LabeledStatement
  | ControlTransferStatement
  | DeferStatement
  | DoStatement
  | CompilerControlStatement
  | WhileStatement
  | RepeatWhileStatement

data ForInit
  = FiDeclaration Declaration
  | FiExpressionList [Expression]

data CodeBlock = CodeBlock

data Pattern = Pattern

data Declaration
  = VariableDeclaration
  | DummyDeclaration

data Type = Type String -- identifier
  deriving (Show, Eq)
