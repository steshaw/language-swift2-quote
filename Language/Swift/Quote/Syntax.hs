module Language.Swift.Quote.Syntax where

import Data.Text (Text)

data Module = Module Expression
  deriving (Show, Eq)

data Expression
  = IntegerLiteral Integer
  | StringLiteral String
  | BooleanLiteral Bool
  | NilLiteral String
  | PrefixExpression PrefixExpression
  | DummyExpression
  deriving (Show, Eq)

data BinaryExpression
  = BinaryExpression1
    { beOperator :: String
    , bePrefixExpression :: PrefixExpression
    }
  | BinaryExpression2
    { beAssignmentOperator :: String
    , beTryOperator :: Maybe String
    , bePrefixExpression :: PrefixExpression
    }
  | BinaryExpression3 (Maybe String, Expression) (Maybe String) PrefixExpression
  | BinaryExpression4 String Type

data PrefixExpression
  = PeRegular (Maybe String) {- prefixOperator -} Expression
  | PeInOutExpression String -- identifier
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

data Type = Type
