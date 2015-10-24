module Language.Swift.Quote.Syntax where

data Module = Module [Statement]
  deriving (Show, Eq)

data Expression
  = Expression (Maybe String) PrefixExpression [BinaryExpression]
  deriving (Show, Eq)

data PrefixExpression
  = PrefixExpression (Maybe String) {- prefixOperator -} PostfixExpression
  | InOutExpression String -- identifier
  deriving (Show, Eq)

data IdG = IdG
  { idgIdentifier :: String
  , idgGenericArgs :: [Type]
  }
  deriving (Show, Eq)

data PostfixExpression
  = PostfixPrimary PrimaryExpression
  | PostfixOperator PostfixExpression String -- postfix-operator
  | ExplicitMemberExpressionDigits PostfixExpression String -- digits
  | ExplicitMemberExpressionIdentifier PostfixExpression IdG
  | FunctionCallE FunctionCall
  | PostfixExpression4Initalizer PostfixExpression
  | PostfixSelf PostfixExpression
  | PostfixDynamicType PostfixExpression
  | PostfixForcedValue PostfixExpression
  | PostfixOptionChaining PostfixExpression
  | Subscript PostfixExpression [Expression]
  deriving (Show, Eq)

data ExpressionElement = ExpressionElement (Maybe String) Expression
  deriving (Show, Eq)

data FunctionCall = FunctionCall PostfixExpression [ExpressionElement] (Maybe Closure)
  deriving (Show, Eq)

data Closure = Closure [Statement]
  deriving (Show, Eq)

data PrimaryExpression
  = PrimaryExpression1 IdG
  | PrimaryLiteral LiteralExpression
  | PrimarySelf SelfExpression
  | PrimarySuper SuperclassExpression
  | PrimaryClosure Closure -- XXX closure-expression
  | PrimaryParenthesized [ExpressionElement] -- parenthesized-expression
  | PrimaryImplicitMember -- TODO implicit-member-expression
  | PrimaryWildcard -- wildcard-expression
  deriving (Show, Eq)

type Identifier = String

data SelfExpression
  = Self
  | SelfMethod Identifier
  | SelfSubscript [Expression]
  | SelfInit
  deriving (Show, Eq)

data SuperclassExpression
  = SuperMethod Identifier
  | SuperSubscript [Expression]
  | SuperInit
  deriving (Show, Eq)

data Literal
  = NumericLiteral String
  | StringLiteral StringLiteral
  | BooleanLiteral Bool
  | NilLiteral
  deriving (Show, Eq)

data StringLiteral
  = StaticStringLiteral String
  | InterpolatedStringLiteral [InterpolatedTextItem]
  deriving (Show, Eq)

data InterpolatedTextItem
  = TextItemString String
  | TextItemExpr Expression
  deriving (Show, Eq)

data LiteralExpression
  = RegularLiteral Literal
  | ArrayLiteral [Expression]
  | DictionaryLiteral [(Expression, Expression)]
  | SpecialLiteral String
  deriving (Show, Eq)

data BinaryExpression
  = BinaryExpression1
    { beOperator :: String
    , bePrefixExpression :: PrefixExpression
    }
  | BinaryAssignmentExpression
    { beTryOperator :: Maybe String
    , bePrefixExpression :: PrefixExpression
    }
  | BinaryConditional (Maybe TryOp, Expression) (Maybe TryOp) PrefixExpression
  | BinaryExpression4 String Type
  deriving (Show, Eq)

type TryOp = String

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
    , fiWhereClause :: Maybe WhereClause
    , fiBlock :: CodeBlock
    }
  | DeclarationStatement Declaration
  | ReturnStatement (Maybe Expression)
  | WhileStatement Expression CodeBlock
  | RepeatWhileStatement CodeBlock Expression
  | GuardStatement ConditionClause CodeBlock
  | SwitchStatement -- TODO
  | IfStatement ConditionClause CodeBlock (Maybe (Either CodeBlock {- if -}Statement))
  | LabelelStatement LabelName Statement
  | BreakStatement (Maybe LabelName)
  | ContinueStatement (Maybe LabelName)
  | FallthroughStatement
  | ThrowStatement Expression
  | DeferStatement CodeBlock
  | DoStatement CodeBlock [CatchClause]
  | LineControlLine
  | LineControlSpecified {- lineNum -} Integer {- fileName -} String
  | BuildConfigurationStatement BuildConfiguration [Statement] [BuildConfigurationElseifClause]
                                (Maybe BuildConfigurationElseClause)
  deriving (Show, Eq)

data CatchClause = CatchClause (Maybe Pattern) (Maybe WhereClause) CodeBlock
  deriving (Show, Eq)

data WhereClause = WhereClause Expression
  deriving (Show, Eq)

type LabelName = String

type ConditionClause = Expression

data ForInit
  = FiDeclaration Declaration
  | FiExpressionList [Expression]
  deriving (Show, Eq)

data CodeBlock = CodeBlock [Statement]
  deriving (Show, Eq)

type Name = String
type TypeAliasName = Name
type StructName = Name
type ClassName = Name
data StructType = Struct | Class
  deriving (Show, Eq)

data Declaration
  = ImportDeclaration [Attribute] (Maybe ImportKind) ImportPath
  | DeclVariableDeclaration VariableDeclaration
  | ConstantDeclaration [Attribute] [DeclarationModifier] [PatternInitializer]
  | TypeAlias [Attribute] (Maybe DeclarationModifier) TypeAliasName Type
  | FunctionDeclaration
    { funAttrs :: [Attribute]
    , funDecls :: [DeclarationModifier]
    , funName :: FunctionName
    , funGenericParamClause :: Maybe GenericParameterClause
    , funParameterClauses :: [[Parameter]]
    , funThrowDecl :: Maybe String -- "throws" or "rethrows"
    , funResult :: Maybe FunctionResult
    , funBody :: Maybe CodeBlock
    }
  | EnumDeclaration EnumDeclaration
  | StructDeclaration
      StructType
      [Attribute]
      (Maybe DeclarationModifier)
      StructName
      (Maybe GenericParameterClause)
      (Maybe TypeInheritanceClause)
      [Declaration]
  | InitializerDeclaration
      [Attribute]
      [DeclarationModifier]
      InitKind
      (Maybe GenericParameterClause)
      [Parameter]
      String -- throwsDecl
      CodeBlock
  | DeinitializerDeclaration
      [Attribute]
      CodeBlock
  deriving (Show, Eq)

data VariableDeclaration
  = VarPatternInitializer [Attribute] [DeclarationModifier] [PatternInitializer]
  | VarSimple [Attribute] [DeclarationModifier] String TypeAnnotation (Maybe Expression)
  deriving (Show, Eq)

type IsIndirect = Bool
type EnumName = String
type TypeInheritanceClause = ()

data EnumDeclaration
  = UnionEnum
      [Attribute]
      (Maybe DeclarationModifier)
      IsIndirect
      EnumName
      (Maybe GenericParameterClause)
      (Maybe TypeInheritanceClause)
      [UnionStyleEnumMember]
  | RawEnum
  deriving (Show, Eq)

type CaseName = String

data UnionStyleEnumMember
    = EnumMemberDeclaration Declaration
    | EnumMemberCase [Attribute] IsIndirect [(CaseName, Maybe {-Tuple-} Type)]
  deriving (Show, Eq)

data DeclarationModifier
  = Modifier String
  | AccessLevelModifier String
  deriving (Show, Eq)

data Attribute = DummyAttribute
  deriving (Show, Eq)

data Type
  = SimpleType Identifier
  | TypeOpt Type
  | ImplicitlyUnwrappedOptType Type
  deriving (Show, Eq)

type ImportPath = [ImportPathIdentifier]
type ImportKind = String

data ImportPathIdentifier
  = ImportIdentifier String
  | ImportOperator String
  deriving (Show, Eq)

type OptInitExpr = Maybe Expression

data PatternInitializer = PatternInitializer Pattern OptInitExpr
  deriving (Show, Eq)

type OptTypeAnnotation = Maybe TypeAnnotation

data Pattern
  = WildcardPattern OptTypeAnnotation
  | IdentifierPattern String OptTypeAnnotation
  | TuplePattern [Pattern] OptTypeAnnotation
  | ExpressionPattern Expression
  deriving (Show, Eq)

data FunctionResult = FunctionResult [Attribute] Type
  deriving (Show, Eq)

data FunctionName
  = FunctionNameIdent String
  | FunctionNameOp String
  deriving (Show, Eq)

data GenericParameterClause = GenericParameterClause [GenericParameter] (Maybe GenericRequirementClause)
  deriving (Show, Eq)

data GenericParameter = GenericParameter String
  deriving (Show, Eq)

data GenericRequirementClause = GenericRequirementClause
  deriving (Show, Eq)

data Parameter
  = ParameterLet (Maybe String) String TypeAnnotation (Maybe Expression)
  | ParameterVar (Maybe String) String TypeAnnotation (Maybe Expression)
  | ParameterInOut (Maybe String) String TypeAnnotation
  | ParameterDots (Maybe String) String TypeAnnotation
  deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation [Attribute] Type
  deriving (Show, Eq)

data BuildConfigurationElseifClause = BuildConfigurationElseifClause BuildConfiguration [Statement]
  deriving (Show, Eq)

data BuildConfigurationElseClause = BuildConfigurationElseClause [Statement]
  deriving (Show, Eq)

data BuildConfiguration
  = OperatingSystemTest String
  | ArchitectureTest String
  | BuildConfigurationId String
  | BuildConfigurationBool Literal
  | BuildConfigurationNegate BuildConfiguration
  | BuildConfigurationOr BuildConfiguration BuildConfiguration
  | BuildConfigurationAnd BuildConfiguration BuildConfiguration
  deriving (Show, Eq)

data InitKind
  = Init
  | InitOption
  | InitForce
  deriving (Show, Eq)
