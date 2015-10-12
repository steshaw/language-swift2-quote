module Language.Swift.Quote.Syntax where

data Module = Module (Maybe [Statement])
  deriving (Show, Eq)

data Expression
  = Expression (Maybe String) PrefixExpression [BinaryExpression]
  deriving (Show, Eq)

data PrefixExpression
  = PrefixExpression1 (Maybe String) {- prefixOperator -} PostfixExpression
  | PrefixExpression2 String -- identifier
  deriving (Show, Eq)

data IdG = IdG
  { idgIdentifier :: String
  , idgGenericArgs :: Maybe [Type]
  }
  deriving (Show, Eq)

data PostfixExpression
  = PostfixExpression1 PrimaryExpression
  | PostfixOperator PostfixExpression String -- postfix-operator
  | ExplicitMemberExpressionDigits PostfixExpression String -- digits
  | ExplicitMemberExpressionIdentifier PostfixExpression IdG
  | FunctionCallE FunctionCall
  | PostfixExpression4Initalizer PrefixExpression
  deriving (Show, Eq)

data ExpressionElement = ExpressionElement (Maybe String) Expression
  deriving (Show, Eq)

data FunctionCall = FunctionCall PostfixExpression [ExpressionElement] (Maybe Closure)
  deriving (Show, Eq)

data Closure = Closure [Statement]
  deriving (Show, Eq)

data PrimaryExpression
  = PrimaryExpression1 IdG
  | PrimaryExpression2 LiteralExpression
  | PrimaryExpression3 SelfExpression
  | PrimaryExpression4 SuperclassExpression
  | PrimaryExpression5 Closure -- XXX closure-expression
  | PrimaryExpression6 [ExpressionElement] -- parenthesized-expression
  | PrimaryExpression7 -- TODO implicit-member-expression
  | PrimaryExpression8 -- wildcard-expression
  deriving (Show, Eq)

data SuperclassExpression = SuperclassExpression -- TODO
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
  = ExpressionStatement Expression
  | ForStatement
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
  | DummyStatement
  deriving (Show, Eq)

data ForInit
  = FiDeclaration Declaration
  | FiExpressionList [Expression]
  deriving (Show, Eq)

data CodeBlock = CodeBlock (Maybe [Statement])
  deriving (Show, Eq)

data Pattern = Pattern
  deriving (Show, Eq)

data Declaration
  = ImportDeclaration (Maybe Attributes) (Maybe ImportKind) ImportPath
  | VariableDeclaration
  | DummyDeclaration
  deriving (Show, Eq)

data Attributes = DummyAttributes
  deriving (Show, Eq)

data Type = Type String -- identifier
  deriving (Show, Eq)

type ImportPath = [ImportPathIdentifier]
type ImportKind = String

data ImportPathIdentifier
  = ImportIdentifier String
  | ImportOperator String
  deriving (Show, Eq)
