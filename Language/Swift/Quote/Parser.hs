{-# LANGUAGE FlexibleContexts #-}

module Language.Swift.Quote.Parser where

import Language.Swift.Quote.Syntax

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Arrow (left)
import Data.Maybe
import Data.Text
import Debug.Trace
import qualified Text.ParserCombinators.Parsec as P
import Text.Parsec.Text (Parser)
import Text.Parsec (try)
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import Text.Parsec.Prim (parserFail)

parseIt :: Parser a -> Text -> Either String a
parseIt p input = left show (P.parse (ws *> p <* ws) "<stdin>" input)

parse :: Text -> Either String Module
parse = parseIt module_

parseExpression :: Text -> Either String Expression
parseExpression = parseIt expression

parseDeclaration :: Text -> Either String Declaration
parseDeclaration = parseIt declaration

parseFunctionCall :: Text -> Either String FunctionCall
parseFunctionCall = parseIt functionCallExpression

parseInitializer :: Text -> Either String PostfixExpression
parseInitializer = parseIt initializerExpression

initializerExpression :: Parser PostfixExpression
initializerExpression = do
  pfe <- postfixExpression
  _ <- op "."
  postfixInitTail pfe

module_ :: Parser Module
module_ = do
  topDecls <- topLevelDeclaration
  _ <- P.eof
  return $ Module topDecls

------------------------------------------------------------
-- Auxiliary
------------------------------------------------------------
attributeList :: Parser [Attribute]
attributeList = fromMaybe [] <$> optional attributes

notice :: String -> String
notice msg = "\n\n\n" ++ msg ++ "\n\n\n"

traceVar n v = trace (notice n ++ " = " ++ show v) $ pure ()

------------------------------------------------------------
-- Lexical Structure (old)
------------------------------------------------------------

reservedWordsDeclarations :: [String]
reservedWordsDeclarations =
 [ "class"
 , "deinit"
 , "enum"
 , "extension"
 , "func"
 , "import"
 , "init"
 , "inout"
 , "internal"
 , "let"
 , "operator"
 , "private"
 , "protocol"
 , "public"
 , "static"
 , "struct"
 , "subscript"
 , "typealias"
 , "var"
 ]

reservedWordsStatements :: [String]
reservedWordsStatements =
  [ "break"
  , "case"
  , "continue"
  , "default"
  , "defer"
  , "do"
  , "else"
  , "fallthrough"
  , "for"
  , "guard"
  , "if"
  , "in"
  , "repeat"
  , "return"
  , "switch"
  , "where"
  , "while"
  ]

reservedWordsExpressionsTypes :: [String]
reservedWordsExpressionsTypes =
  [ "as"
  , "catch"
  , "dynamicType"
  , "false"
  , "is"
  , "nil"
  , "rethrows"
  , "super"
  , "self"
  , "Self"
  , "throw"
  , "throws"
  , "true"
  , "try"
  , "__COLUMN__"
  , "__FILE__"
  , "__FUNCTION__"
  , "__LINE__"
  ]

keywordsInPatterns :: [String]
keywordsInPatterns = pure "_"

keywordsinContexts :: [String]
keywordsinContexts =
  [ "associativity"
  , "convenience"
  , "dynamic"
  , "didSet"
  , "final"
  , "get"
  , "infix"
  , "indirect"
  , "lazy"
  , "left"
  , "mutating"
  , "none"
  , "nonmutating"
  , "optional"
  , "override"
  , "postfix"
  , "precedence"
  , "prefix"
  , "Protocol"
  , "required"
  , "right"
  , "set"
  , "Type"
  , "unowned"
  , "weak"
  , "willSet"
  ]

-- reservedOperators :: [String]
-- reservedOperators = []
--
-- legalOpChars :: String
-- legalOpChars = ":!#$%&*+./<=>?@\\^|-~"

-- Ignore Whitespace
-- C-style comments with nesting.
swiftLangDef :: L.GenLanguageDef Text st Identity
swiftLangDef = T.LanguageDef
  { T.commentStart = "/*"
  , T.commentEnd = "*/"
  , T.commentLine = "//"
  , T.nestedComments = True
  , T.identStart = P.letter
  , T.identLetter = P.alphaNum
  , T.reservedNames = reservedWordsDeclarations
                          ++ reservedWordsStatements
                          ++ reservedWordsExpressionsTypes
                          ++ keywordsInPatterns
                          ++ keywordsinContexts
  , T.opStart = P.oneOf ""
  , T.opLetter = P.oneOf ""
  , T.reservedOpNames = []
  , T.caseSensitive = True
  }

lexer :: T.GenTokenParser Text u Identity
lexer = T.makeTokenParser swiftLangDef

ws :: Parser ()
ws = T.whiteSpace lexer

comma :: Parser String
comma = T.comma lexer

colon :: Parser String
colon = T.colon lexer

semicolon :: Parser String
semicolon = T.semi lexer

optSemicolon :: Parser (Maybe String)
optSemicolon = optional semicolon

braces :: Parser a -> Parser a
braces p = do
  tok "{"
  a <- p
  tok "}"
  return a

parens :: Parser a -> Parser a
parens = T.parens lexer

brackets :: Parser a -> Parser a
brackets = T.brackets lexer

angles :: Parser a -> Parser a
angles = T.angles lexer

identifier :: Parser String
identifier = T.identifier lexer

kw :: String -> Parser ()
kw s = ws *> T.reserved lexer s

kw' :: String -> Parser String
kw' s = kw s *> pure s

------------------------------------------------------------
-- Auxiliary
------------------------------------------------------------

op :: String -> Parser ()
op s = op' s *> pure ()

op' :: String -> Parser String
op' s = try $ do
  o <- operator
  when (s /= o) $ fail ("Expecting operator " ++ s ++ " but got " ++ o)
  return o

tok' :: String -> Parser String
tok' s = try (ws *> P.string s <* ws)

tok :: String -> Parser ()
tok s = tok' s *> pure ()

------------------------------------------------------------
-- SUMMARY OF THE GRAMMAR
------------------------------------------------------------

whereClause :: Parser Expression
whereClause = expression -- TODO

------------------------------------------------------------
-- Statements
------------------------------------------------------------

-- GRAMMAR OF A STATEMENT
statement :: Parser Statement
statement
    = DeclarationStatement <$> declaration <* optSemicolon
  <|> ExpressionStatement <$> expression <* optSemicolon
  <|> loopStatement <* optSemicolon
  <|> branchStatement <* optSemicolon
  <|> labeledStatement <* optSemicolon
  <|> controlTransferStatement <* optSemicolon
  -- <|> deferStatement <* optSemicolon
  -- <|> doStatement <* optSemicolon
  -- <|> compilerControlStatement <* optSemicolon

statements :: Parser [Statement]
statements = many statement

-- GRAMMAR OF A LOOP STATEMENT
loopStatement :: Parser Statement
loopStatement
    = kw "for" *> (forStatementTail <|> forInStatementTail)
  <|> whileStatement
  <|> repeatWhileStatement

forStatementTail :: Parser Statement
forStatementTail
    = do
        (i, e1, e2) <- forMiddle
        b <- codeBlock
        return $ ForStatement i e1 e2 b
  <|> do
        _ <- kw "for"
        (i, e1, e2) <- parens forMiddle
        b <- codeBlock
        return $ ForStatement i e1 e2 b

forMiddle :: Parser (Maybe ForInit, Maybe Expression, Maybe Expression)
forMiddle = do
          i <- optional forInit
          _ <- semicolon
          e1 <- optional expression
          _ <- semicolon
          e2 <- optional expression
          return (i, e1, e2)

forInit :: Parser ForInit
forInit
    = FiDeclaration <$> variableDeclaration
  <|> FiExpressionList <$> expressionList

forInStatementTail :: Parser Statement
forInStatementTail = do
  _ <- optional $ kw "case"
  p <- pattern
  _ <- kw "in"
  e <- expression
  w <- optional whereClause
  b <- codeBlock
  pure $ ForInStatement p e w b

-- GRAMMAR OF A WHILE STATEMENT

whileStatement :: Parser Statement
whileStatement = kw "while" *> (WhileStatement <$> conditionClause <*> codeBlock)

conditionClause :: Parser Expression
conditionClause = expression
-- condition-clause → expression­,­condition-list­
-- condition-clause → condition-list­
-- condition-clause → availability-condition­,­expression­
-- condition-list → condition­  condition­,­condition-list­
-- condition → availability-condition­  case-condition­  optional-binding-condition­
-- case-condition → case­pattern­initializer­where-clause­opt­
-- optional-binding-condition → optional-binding-head­optional-binding-continuation-list­opt­where-clause­opt­
-- optional-binding-head → let­pattern­initializer­  var­pattern­initializer­
-- optional-binding-continuation-list → optional-binding-continuation­ optional-binding-continuation­,­optional-binding-continuation-list­
-- optional-binding-continuation → pattern­initializer­  optional-binding-head­

-- GRAMMAR OF A REPEAT-WHILE STATEMENT
repeatWhileStatement :: Parser Statement
repeatWhileStatement = kw "repeat" *> (RepeatWhileStatement <$> codeBlock <* kw "while" <*> expression)

-- GRAMMAR OF A BRANCH STATEMENT

branchStatement :: Parser Statement
branchStatement = ifStatement <|> guardStatement <|> switchStatement

-- GRAMMAR OF AN IF STATEMENT

ifStatement :: Parser Statement
ifStatement = do
  _ <- kw "if"
  c <- conditionClause
  b <- codeBlock
  e <- optional elseClause
  return $ IfStatement c b e

elseClause :: Parser (Either CodeBlock Statement)
elseClause = do
  _ <- kw "else"
  (Left <$> codeBlock) <|> (Right <$> ifStatement)

-- GRAMMAR OF A GUARD STATEMENT

guardStatement :: Parser Statement
guardStatement = do
  _ <- kw "guard"
  c <- conditionClause
  _ <- kw "else"
  b <- codeBlock
  return $ GuardStatement c b

-- GRAMMAR OF A SWITCH STATEMENT

switchStatement :: Parser Statement
switchStatement = kw "switch" *> pure SwitchStatement
-- switch-statement → switch­expression­{­switch-cases­opt­}­
-- switch-cases → switch-case­switch-cases­opt­
-- switch-case → case-label­statements­  default-label­statements­
-- case-label → case­case-item-list­:­
-- case-item-list → pattern­where-clause­opt­  pattern­where-clause­opt­,­case-item-list­
-- default-label → default­:­
-- where-clause → where­where-expression­
-- where-expression → expression­

-- GRAMMAR OF A LABELED STATEMENT
labeledStatement :: Parser Statement
labeledStatement = LabelelStatement <$> statementLabel <*> (loopStatement <|> ifStatement <|> switchStatement)

statementLabel :: Parser String
statementLabel = labelName <* tok ":"

labelName :: Parser String
labelName = identifier

-- GRAMMAR OF A CONTROL TRANSFER STATEMENT

controlTransferStatement :: Parser Statement
controlTransferStatement = returnStatement <|> breakStatement

-- control-transfer-statement → continue-statement­
-- control-transfer-statement → fallthrough-statement­
-- control-transfer-statement → return-statement­
-- control-transfer-statement → throw-statement­

-- GRAMMAR OF A BREAK STATEMENT
breakStatement :: Parser Statement
breakStatement = kw "break" *> (BreakStatement <$> optional labelName)

--GRAMMAR OF A CONTINUE STATEMENT
continueStatement :: Parser Statement
continueStatement = kw "continue" *> (ContinueStatement <$> optional labelName)

-- GRAMMAR OF A FALLTHROUGH STATEMENT
fallthroughStatement = kw "fallthrough" *> pure FallthroughStatement

-- GRAMMAR OF A RETURN STATEMENT
returnStatement :: Parser Statement
returnStatement = kw "return" *> (ReturnStatement <$> optional expression)

{-
GRAMMAR OF AN AVAILABILITY CONDITION

availability-condition → #available­(­availability-arguments­)­
availability-arguments → availability-argument­  availability-argument­,­availability-arguments­
availability-argument → platform-name­platform-version­
availability-argument → *­
platform-name → iOS­  iOSApplicationExtension­
platform-name → OSX­  OSXApplicationExtension­
platform-name → watchOS­
platform-version → decimal-digits­
platform-version → decimal-digits­.­decimal-digits­
platform-version → decimal-digits­.­decimal-digits­.­decimal-digits­
GRAMMAR OF A THROW STATEMENT

throw-statement → throw­expression­
GRAMMAR OF A DEFER STATEMENT

defer-statement → defer­code-block­
GRAMMAR OF A DO STATEMENT

do-statement → do­code-block­catch-clauses­opt­
catch-clauses → catch-clause­catch-clauses­opt­
catch-clause → catch­pattern­opt­where-clause­opt­code-block­
GRAMMAR OF A COMPILER CONTROL STATEMENT

compiler-control-statement → build-configuration-statement­
compiler-control-statement → line-control-statement­
GRAMMAR OF A BUILD CONFIGURATION STATEMENT

build-configuration-statement → #if­build-configuration­statements­opt­build-configuration-elseif-clauses­opt­build-configuration-else-clause­opt­#endif­
build-configuration-elseif-clauses → build-configuration-elseif-clause­build-configuration-elseif-clauses­opt­
build-configuration-elseif-clause → #elseif­build-configuration­statements­opt­
build-configuration-else-clause → #else­statements­opt­
build-configuration → platform-testing-function­
build-configuration → identifier­
build-configuration → boolean-literal­
build-configuration → (­build-configuration­)­
build-configuration → !­build-configuration­
build-configuration → build-configuration­&&­build-configuration­
build-configuration → build-configuration­||­build-configuration­
platform-testing-function → os­(­operating-system­)­
platform-testing-function → arch­(­architecture­)­
operating-system → OSX­  iOS­  watchOS­  tvOS­
architecture → i386­  x86_64­  arm­  arm64­
GRAMMAR OF A LINE CONTROL STATEMENT

line-control-statement → #line­
line-control-statement → #line­line-number­file-name­
line-number → A decimal integer greater than zero
file-name → static-string-literal­
-}

------------------------------------------------------------
-- Generic Parameters and Arguments
------------------------------------------------------------

-- GRAMMAR OF A GENERIC PARAMETER CLAUSE

genericParameterClause :: Parser GenericParameterClause
genericParameterClause
  = angles (GenericParameterClause <$> genericParameterList <*> optional requirementClause)

genericParameterList :: Parser [GenericParameter]
genericParameterList = genericParameter `P.sepBy1` comma

genericParameter :: Parser GenericParameter
genericParameter = GenericParameter <$> identifier

requirementClause :: Parser GenericRequirementClause
requirementClause = pure GenericRequirementClause

-- generic-parameter → type-name­:­type-identifier­
-- generic-parameter → type-name­:­protocol-composition-type­
-- requirement-clause → where­requirement-list­
-- requirement-list → requirement­  requirement­,­requirement-list­
-- requirement → conformance-requirement­  same-type-requirement­
-- conformance-requirement → type-identifier­:­type-identifier­
-- conformance-requirement → type-identifier­:­protocol-composition-type­
-- same-type-requirement → type-identifier­==­type­

-- GRAMMAR OF A GENERIC ARGUMENT CLAUSE
genericArgumentClause :: Parser [Type]
genericArgumentClause = angles (P.many1 type_)

------------------------------------------------------------
-- Declarations
------------------------------------------------------------

-- GRAMMAR OF A DECLARATION
declaration :: Parser Declaration
declaration
    = importDeclaration
  <|> try constantDeclaration
  <|> try variableDeclaration
  <|> try typealiasDeclaration
  <|> try functionDeclaration
  <|> try enumDeclaration
  <|> try structDeclaration
  <|> try classDeclaration
{-
declaration → protocol-declaration­
declaration → initializer-declaration­
declaration → deinitializer-declaration­
declaration → extension-declaration­
declaration → subscript-declaration­
declaration → operator-declaration­
declarations → declaration­declarations­opt­
-}

-- GRAMMAR OF A TOP-LEVEL DECLARATION

topLevelDeclaration :: Parser [Statement]
topLevelDeclaration = fromMaybe [] <$> optional statements

-- GRAMMAR OF A CODE BLOCK

codeBlock :: Parser CodeBlock
codeBlock = CodeBlock <$> braces (fromMaybe [] <$> optional statements)

-- GRAMMAR OF AN IMPORT DECLARATION

importDeclaration :: Parser Declaration
importDeclaration
  = ImportDeclaration
      <$> attributeList
      <*  kw "import"
      <*> optional importKind
      <*> importPath

attributes :: Parser [Attribute]
attributes = P.many attribute

importKind :: Parser ImportKind
importKind = P.choice
  [ kw' "typealias"
  , kw' "struct"
  , kw' "class"
  , kw' "enum"
  , kw' "protocol"
  , kw' "var"
  , kw' "func"
  ]

importPath :: Parser ImportPath
importPath = importPathIdentifier `P.sepBy1 ` tok "."

importPathIdentifier :: Parser ImportPathIdentifier
importPathIdentifier
    = ImportIdentifier <$> identifier
  <|> ImportOperator <$> operator

-- GRAMMAR OF A CONSTANT DECLARATION

constantDeclaration :: Parser Declaration
constantDeclaration = do
  atts <- attributeList
  mods <- fromMaybe [] <$> optional declarationModifiers
  _ <- kw "let"
  is <- patternInitializerList
  return $ ConstantDeclaration atts mods is

patternInitializerList :: Parser [PatternInitializer]
patternInitializerList = patternInitializer `P.sepBy1` comma

patternInitializer :: Parser PatternInitializer
patternInitializer = PatternInitializer <$> pattern <*> optional initializer

initializer :: Parser Expression
initializer = tok "=" *> expression

-- GRAMMAR OF A VARIABLE DECLARATION
variableDeclaration :: Parser Declaration
variableDeclaration = DeclVariableDeclaration <$> variableDeclarationBody

variableDeclarationBody :: Parser VariableDeclaration
variableDeclarationBody
  = do
    (attrs, mods) <- variableDeclarationHead
    -- (try (VarPatternInitializer attrs mods <$> patternInitializerList))
    --   <|> variableDeclarationName attrs mods

    try (variableDeclarationName attrs mods) <|>
      (VarPatternInitializer attrs mods <$> patternInitializerList)

variableDeclarationName :: [Attribute] -> [DeclarationModifier] -> Parser VariableDeclaration
variableDeclarationName attrs mods = do
  n <- variableName
  t <- typeAnnotation
  i <- optional initializer
  -- variable-declaration → ­­type-annotation (­initializer ­opt) ­willSet-didSet-block­
  -- variable-declaration → ­­type-annotation­ code-block­
  -- variable-declaration → ­­type-annotation­ getter-setter-block­
  -- variable-declaration → ­­type-annotation­ getter-setter-keyword-block­
  -- variable-declaration → ­­initializer­ willSet-didSet-block­
  return $ VarSimple attrs mods n t i

variableDeclarationHead :: Parser ([Attribute], [DeclarationModifier])
variableDeclarationHead = do
  attrs <- attributeList
  mods <- fromMaybe [] <$> optional declarationModifiers
  _ <- kw "var"
  return (attrs, mods)

variableName :: Parser String
variableName = identifier

getterSetterBlock = codeBlock
-- getter-setter-block → code-block­
-- getter-setter-block → {­getter-clause­setter-clause­opt­}­
-- getter-setter-block → {­setter-clause­getter-clause­}­

-- getter-clause → attributes­opt­get­code-block­

-- setter-clause → attributes­opt­set­setter-name­opt­code-block­

-- setter-name → (­identifier­)­

-- getter-setter-keyword-block → {­getter-keyword-clause­setter-keyword-clause­opt­}­
-- getter-setter-keyword-block → {­setter-keyword-clause­getter-keyword-clause­}­

-- getter-keyword-clause → attributes­opt­get­

-- setter-keyword-clause → attributes­opt­set­

-- willSet-didSet-block → {­willSet-clause­didSet-clause­opt­}­
-- willSet-didSet-block → {­didSet-clause­willSet-clause­opt­}­

-- willSet-clause → attributes­opt­willSet­setter-name­opt­code-block­

-- didSet-clause → attributes­opt­didSet­setter-name­opt­code-block­

-- GRAMMAR OF A TYPE ALIAS DECLARATION
typealiasDeclaration :: Parser Declaration
typealiasDeclaration = do
  (atts, m, name) <- typealiasHead
  t <-  typealiasAssignment
  return (TypeAlias atts m name t)

typealiasHead :: Parser ([Attribute], Maybe DeclarationModifier, String)
typealiasHead = do
  atts <- attributeList
  m <- optional accessLevelModifier
  _ <- kw "typealias"
  name <- typealiasName
  return (atts, m, name)

typealiasName :: Parser String
typealiasName = identifier

typealiasAssignment :: Parser Type
typealiasAssignment = tok "=" *> type_

-- GRAMMAR OF A FUNCTION DECLARATION
functionDeclaration :: Parser Declaration
functionDeclaration = do
   (attr, declMods) <- functionHead
   n <- functionName
   gs <- optional genericParameterClause
   (p, t, r) <- functionSignature
   b <- optional functionBody
   return $ FunctionDeclaration attr declMods n gs p t r b

functionHead :: Parser ([Attribute], [DeclarationModifier])
functionHead = do
  a <- attributeList
  m <- fromMaybe [] <$> optional declarationModifiers
  _ <- kw "func"
  return (a, m)

functionName :: Parser FunctionName
functionName
    = (FunctionNameIdent <$> identifier)
  <|> (FunctionNameOp <$> operator)

functionSignature :: Parser ([[Parameter]], Maybe String, Maybe FunctionResult)
functionSignature = do
  p <- parameterClauses
  t <- optional (kw' "throws" <|> kw' "rethrows")
  r <- optional functionResult
  return (p, t, r)

functionResult :: Parser FunctionResult
functionResult = op "->" *> (FunctionResult <$> attributeList <*> type_)

functionBody :: Parser CodeBlock
functionBody = codeBlock

parameterClauses :: Parser [[Parameter]]
parameterClauses = many parameterClause

parameterClause :: Parser [Parameter]
parameterClause
    = try (parens (pure []))
  <|> parens parameterList

parameterList :: Parser [Parameter]
parameterList = parameter `P.sepBy1` comma

parameter :: Parser Parameter
parameter
    = do
        -- _ <- trace "in parameter production 1" $ pure ()
        _ <- optional (kw "let")
        fn <- parameterName
        -- _ <- trace "in parameter 2" $ pure ()
        sn <- optional parameterName
        -- _ <- trace "in parameter 3" $ pure ()
        let (extern, local) = if isJust sn then (Just fn, fromJust sn) else (Nothing, fn)
        -- _ <- trace "in parameter 3" $ pure ()
        t <- typeAnnotation
        -- _ <- trace "in parameter 4" $ pure ()
        c <- optional defaultArgumentClause
        -- _ <- trace "in parameter 5" $ pure ()
        return $ ParameterLet extern local t c
  <|> do
        _ <- kw "var"
        extern <- optional externalParameterName
        local  <- localParameterName
        t <- typeAnnotation
        c <- optional defaultArgumentClause
        return $ ParameterVar extern local t c
  <|> do
        _ <- kw "inout"
        extern <- optional externalParameterName
        local  <- localParameterName
        t <- typeAnnotation
        return $ ParameterInOut extern local t
  <|> do -- FIXME move detection to first production
        extern <- optional externalParameterName
        local  <- localParameterName
        t <- typeAnnotation
        _ <- tok "..."
        return $ ParameterDots extern local t

parameterName :: Parser String
parameterName = identifier <|> op' "_"

externalParameterName :: Parser String
externalParameterName = parameterName

localParameterName :: Parser String
localParameterName = parameterName

defaultArgumentClause :: Parser Expression
defaultArgumentClause = tok "=" *> expression

-- GRAMMAR OF AN ENUMERATION DECLARATION
enumDeclaration :: Parser Declaration
enumDeclaration = EnumDeclaration <$> do
  atts <- attributeList
  optMod <- optional accessLevelModifier
  unionStyleEnum atts optMod <|> rawValueStyleEnum atts optMod

unionStyleEnum :: [Attribute] -> (Maybe DeclarationModifier) -> Parser EnumDeclaration
unionStyleEnum atts optMod = do
  si <- optional (kw "indirect")
  let i = isJust si
  kw "enum"
  n <- enumName
  optP <- optional genericParameterClause
  ti <- optional typeInheritanceClause
  tok "{"
  ms <- P.many unionStyleEnumMember
  tok "}"
  return $ UnionEnum atts optMod i n optP ti ms

typeInheritanceClause :: Parser TypeInheritanceClause
typeInheritanceClause = fail "WIP"

unionStyleEnumMember :: Parser UnionStyleEnumMember
unionStyleEnumMember
    = EnumMemberDeclaration <$> declaration
  <|> unionStyleEnumCaseClause

unionStyleEnumCaseClause :: Parser UnionStyleEnumMember
unionStyleEnumCaseClause = do
  atts <- attributeList
  si <- optional (kw "indirect")
  let i = isJust si
  kw "case"
  cs <- unionStyleEnumCaseList
  return $ EnumMemberCase atts i cs

unionStyleEnumCaseList :: Parser [(EnumName, Maybe Type)]
unionStyleEnumCaseList = unionStyleEnumCase `P.sepBy` comma

unionStyleEnumCase :: Parser (EnumName, Maybe Type)
unionStyleEnumCase = do
  n <- enumCaseName
  tt <- optional tupleType
  return (n, tt)

tupleType :: Parser Type
tupleType = fail "WIP tupleType"

enumName :: Parser String
enumName = identifier

enumCaseName :: Parser String
enumCaseName = identifier

rawValueStyleEnum :: [Attribute] -> (Maybe DeclarationModifier) -> Parser EnumDeclaration
rawValueStyleEnum att optMod = fail "WIP rawValueStyleEnum"
-- raw-value-style-enum → enum­enum-name­generic-parameter-clause­opt­type-inheritance-clause­{­raw-value-style-enum-members­}­
-- raw-value-style-enum-members → raw-value-style-enum-member­raw-value-style-enum-members­opt­
-- raw-value-style-enum-member → declaration­  raw-value-style-enum-case-clause­
-- raw-value-style-enum-case-clause → attributes­opt­case­raw-value-style-enum-case-list­
-- raw-value-style-enum-case-list → raw-value-style-enum-case­ raw-value-style-enum-case­,­raw-value-style-enum-case-list­
-- raw-value-style-enum-case → enum-case-name­raw-value-assignment­opt­
-- raw-value-assignment → =­raw-value-literal­
-- raw-value-literal → numeric-literal­  static-string-literal­  boolean-literal­

structDeclaration' keyword ctor = do
  atts <- attributeList
  optMod <- optional accessLevelModifier
  kw keyword
  n <- structName
  optGPC <- optional genericParameterClause
  optTIC <- optional typeInheritanceClause
  decls <- structBody
  return $ ctor atts optMod n optGPC optTIC decls

structBody :: Parser [Declaration]
structBody = do
  tok "{"
  ms <- P.many declaration
  tok "}"
  return ms

structName :: Parser String
structName = identifier

-- GRAMMAR OF A STRUCTURE DECLARATION

structDeclaration :: Parser Declaration
structDeclaration = structDeclaration' "struct" (StructDeclaration Struct)

-- GRAMMAR OF A CLASS DECLARATION

classDeclaration :: Parser Declaration
classDeclaration = structDeclaration' "class" (StructDeclaration Class)

{-
GRAMMAR OF A PROTOCOL DECLARATION

protocol-declaration → attributes­opt­access-level-modifier­opt­protocol­protocol-name­type-inheritance-clause­opt­protocol-body­
protocol-name → identifier­
protocol-body → {­protocol-member-declarations­opt­}­
protocol-member-declaration → protocol-property-declaration­
protocol-member-declaration → protocol-method-declaration­
protocol-member-declaration → protocol-initializer-declaration­
protocol-member-declaration → protocol-subscript-declaration­
protocol-member-declaration → protocol-associated-type-declaration­
protocol-member-declarations → protocol-member-declaration­protocol-member-declarations­opt­
GRAMMAR OF A PROTOCOL PROPERTY DECLARATION

protocol-property-declaration → variable-declaration-head­variable-name­type-annotation­getter-setter-keyword-block­
GRAMMAR OF A PROTOCOL METHOD DECLARATION

protocol-method-declaration → function-head­function-name­generic-parameter-clause­opt­function-signature­
GRAMMAR OF A PROTOCOL INITIALIZER DECLARATION

protocol-initializer-declaration → initializer-head­generic-parameter-clause­opt­parameter-clause­throws­opt­
protocol-initializer-declaration → initializer-head­generic-parameter-clause­opt­parameter-clause­rethrows­
GRAMMAR OF A PROTOCOL SUBSCRIPT DECLARATION

protocol-subscript-declaration → subscript-head­subscript-result­getter-setter-keyword-block­
GRAMMAR OF A PROTOCOL ASSOCIATED TYPE DECLARATION

protocol-associated-type-declaration → typealias-head­type-inheritance-clause­opt­typealias-assignment­opt­
GRAMMAR OF AN INITIALIZER DECLARATION

initializer-declaration → initializer-head­generic-parameter-clause­opt­parameter-clause­throws­opt­initializer-body­
initializer-declaration → initializer-head­generic-parameter-clause­opt­parameter-clause­rethrows­initializer-body­
initializer-head → attributes­opt­declaration-modifiers­opt­init­
initializer-head → attributes­opt­declaration-modifiers­opt­init­?­
initializer-head → attributes­opt­declaration-modifiers­opt­init­!­
initializer-body → code-block­
GRAMMAR OF A DEINITIALIZER DECLARATION

deinitializer-declaration → attributes­opt­deinit­code-block­
GRAMMAR OF AN EXTENSION DECLARATION

extension-declaration → access-level-modifier­opt­extension­type-identifier­type-inheritance-clause­opt­extension-body­
extension-body → {­declarations­opt­}­
GRAMMAR OF A SUBSCRIPT DECLARATION

subscript-declaration → subscript-head­subscript-result­code-block­
subscript-declaration → subscript-head­subscript-result­getter-setter-block­
subscript-declaration → subscript-head­subscript-result­getter-setter-keyword-block­
subscript-head → attributes­opt­declaration-modifiers­opt­subscript­parameter-clause­
subscript-result → ->­attributes­opt­type­
GRAMMAR OF AN OPERATOR DECLARATION

operator-declaration → prefix-operator-declaration­  postfix-operator-declaration­ infix-operator-declaration­
prefix-operator-declaration → prefix­operator­operator­{­}­
postfix-operator-declaration → postfix­operator­operator­{­}­
infix-operator-declaration → infix­operator­operator­{­infix-operator-attributes­opt­}­
infix-operator-attributes → precedence-clause­opt­associativity-clause­opt­
precedence-clause → precedence­precedence-level­
precedence-level → A decimal integer between 0 and 255, inclusive
associativity-clause → associativity­associativity­
associativity → left­  right­  none­
-}

-- GRAMMAR OF A DECLARATION MODIFIER

declarationModifier :: Parser DeclarationModifier
declarationModifier = modifier <|> accessLevelModifier

declarationModifiers :: Parser [DeclarationModifier]
declarationModifiers = P.many1 declarationModifier

modifier :: Parser DeclarationModifier
modifier = Modifier <$> P.choice
  [ kw' "class"
  , kw' "convenience"
  , kw' "dynamic"
  , kw' "final"
  , kw' "infix"
  , kw' "lazy"
  , kw' "mutating"
  , kw' "nonmutating"
  , kw' "optional"
  , kw' "override"
  , kw' "postfix"
  , kw' "prefix"
  , kw' "required"
  , kw' "static"
  , unowned
  , kw' "weak"
  ]

unowned :: Parser String
unowned = P.choice
    [ try (unownedP "safe")
    , try (unownedP "unsafe")
    , kw' "unowned"
    ]
  where
    unownedP :: String -> Parser String
    unownedP t = do
      k <- kw' "unowned"
      l <- op' "("
      s <- kw' t
      r <- op' ")"
      pure (Prelude.concat [k, l, s, r])

accessLevelModifier :: Parser DeclarationModifier
accessLevelModifier = do
  i <- kw' "internal" <|> kw' "private" <|> kw' "public"
  p <- fromMaybe "" <$> optional setInParens
  return $ Modifier (i ++ p)
  where
    setInParens :: Parser String
    setInParens = parens (kw "set") *> pure "(set)"

------------------------------------------------------------
-- Patterns
------------------------------------------------------------

-- GRAMMAR OF A PATTERN
pattern :: Parser Pattern
pattern
    = wildcardPattern *> (WildcardPattern <$> optional typeAnnotation)
-- pattern → value-binding-pattern­
  <|> TuplePattern <$> tuplePattern <*> optional typeAnnotation
  <|> IdentifierPattern <$> identifierPattern <*> optional typeAnnotation
  <|> ExpressionPattern <$> expression
-- pattern → tuple-pattern­type-annotation­opt­
-- pattern → enum-case-pattern­
-- pattern → optional-pattern­
-- pattern → type-casting-pattern­

-- GRAMMAR OF A WILDCARD PATTERN
wildcardPattern :: Parser ()
wildcardPattern = kw "_"

-- GRAMMAR OF AN IDENTIFIER PATTERN
identifierPattern :: Parser String
identifierPattern = identifier

-- GRAMMAR OF A VALUE-BINDING PATTERN

-- value-binding-pattern → var­pattern­  let­pattern­

-- GRAMMAR OF A TUPLE PATTERN
tuplePattern :: Parser [Pattern]
tuplePattern = parens tuplePatterns

tuplePatterns :: Parser [Pattern]
tuplePatterns = tuplePatternElement `P.sepBy` comma

tuplePatternElement :: Parser Pattern
tuplePatternElement = pattern

{-
GRAMMAR OF AN ENUMERATION CASE PATTERN

enum-case-pattern → type-identifier­opt­.­enum-case-name­tuple-pattern­opt­
GRAMMAR OF AN OPTIONAL PATTERN

optional-pattern → identifier-pattern­?­
GRAMMAR OF A TYPE CASTING PATTERN

type-casting-pattern → is-pattern­  as-pattern­
is-pattern → is­type­
as-pattern → pattern­as­type­
GRAMMAR OF AN EXPRESSION PATTERN

expression-pattern → expression­
-}

------------------------------------------------------------
-- Attributes
------------------------------------------------------------

-- GRAMMAR OF AN ATTRIBUTE
attribute :: Parser Attribute
attribute = parserFail "attribute not implemented" -- TODO
{-
attribute → @­attribute-name­attribute-argument-clause­opt­
attribute-name → identifier­
attribute-argument-clause → (­balanced-tokens­opt­)­
attributes → attribute­attributes­opt­
balanced-tokens → balanced-token­balanced-tokens­opt­
balanced-token → (­balanced-tokens­opt­)­
balanced-token → [­balanced-tokens­opt­]­
balanced-token → {­balanced-tokens­opt­}­
balanced-token → Any identifier, keyword, literal, or operator
balanced-token → Any punctuation except (­, )­, [­, ]­, {­, or }­
-}

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

-- GRAMMAR OF AN EXPRESSION

expression :: Parser Expression
-- expression = Expression
--   <$> optional tryOperator
--   <*> prefixExpression
--   <*> (fromMaybe [] <$> optional binaryExpressions)

expression = do
  t <- optional tryOperator
  -- _ <- trace ("\n\n\n in expression  t = " ++ show t) $ pure ()
  p <- prefixExpression
  -- _ <- trace ("\n  prefix = " ++ show p) $ pure ()
  -- s1 <- try . P.lookAhead $ many P.anyChar
  -- _ <- trace ("ahead s1 = " ++ show s1) $ pure ()
  -- bs <- (fromMaybe [] <$> optional binaryExpressions)
  bs <- optional binaryExpressions
  -- _ <- trace ("\n  bs = " ++ show bs) $ pure ()
  s2 <- try . P.lookAhead $ many P.anyChar
  -- _ <- trace ("ahead s2 = " ++ show s2) $ pure ()
  -- _ <- trace ("\n  binaries = " ++ show bs) $ pure ()
  return $ Expression t p (fromMaybe [] bs)

expressionList :: Parser [Expression]
expressionList = expression `P.sepBy1` comma

-- GRAMMAR OF A PREFIX EXPRESSION

prefixExpression :: Parser PrefixExpression
prefixExpression = try inOutExpression <|> prefixExpression1

prefixExpression1 :: Parser PrefixExpression
prefixExpression1 = do
  o <- optional prefixOperator
  pe <- postfixExpression
  return $ PrefixExpression o pe

inOutExpression :: Parser PrefixExpression
inOutExpression = InOutExpression <$> (op "&" *> identifier)

-- GRAMMAR OF A TRY EXPRESSION

tryOperator :: Parser String
tryOperator
    = kw' "try"
  <|> kw' "try?"
  <|> kw' "try!"

-- GRAMMAR OF A BINARY EXPRESSION

binaryExpression :: Parser BinaryExpression
binaryExpression
    = do
        co <- conditionalOperator
        to <- optional tryOperator
        pe <- prefixExpression
        return $ BinaryConditional co to pe
 <|> do
        _ <- assignmentOperator
        to <- optional tryOperator
        pe <- prefixExpression
        return $ BinaryAssignmentExpression to pe
  <|> typeCastingOperator
  <|> do
        -- _ <- trace "\n\n\nin 2nd case" $ pure ()
        o <- binaryOperator
        -- _ <- trace ("\nbinaryOperator = " ++ show o) $ pure ()
        -- s2 <- try . P.lookAhead $ many P.anyChar
        -- _ <- trace ("\ntrying for prefixExpression in binaryExpression lookAhead s2 = " ++ show s2) $ pure ()
        e <- prefixExpression
        -- _ <- trace ("\nprefix = " ++ show e) $ pure ()
        return $ BinaryExpression1 o e

binaryExpressions :: Parser [BinaryExpression]
binaryExpressions = many binaryExpression

-- GRAMMAR OF AN ASSIGNMENT OPERATOR
assignmentOperator :: Parser ()
assignmentOperator = try $ do
  ws
  _ <- P.char '='
  try (P.notFollowedBy operatorCharacter)
  ws
  return ()

-- GRAMMAR OF A CONDITIONAL OPERATOR
conditionalOperator :: Parser (Maybe String, Expression)
conditionalOperator = do
  _ <- op "?"
  to <- optional tryOperator
  e <- expression
  _ <- tok ":"
  return (to, e)

-- GRAMMAR OF A TYPE-CASTING OPERATOR
typeCastingOperator :: Parser BinaryExpression
typeCastingOperator
    = try (BinaryExpression4 <$> kw' "is" <*> type_)
  <|> try (BinaryExpression4 <$> kw' "as" <*> type_)
  <|> try (do
        k <- kw' "as"
        o <- op' "?"
        t <- type_
        return $ BinaryExpression4 (k ++ o) t)
  <|> try (do
        k <- kw' "as"
        o <- op' "!"
        t <- type_
        return $ BinaryExpression4 (k ++ o) t
      )

-- GRAMMAR OF A PRIMARY EXPRESSION
primaryExpression :: Parser PrimaryExpression
primaryExpression
    = PrimaryExpression1 <$> (IdG <$> identifier <*> (fromMaybe [] <$> optional genericArgumentClause))
  <|> PrimaryLiteral <$> literalExpression
  <|> PrimarySelf <$> selfExpression
  <|> PrimarySuper <$> superclassExpression
  <|> PrimaryClosure <$> closureExpression
  <|> PrimaryParenthesized <$> parenthesizedExpression
  <|> pure PrimaryImplicitMember <* implicitMemberExpression
  <|> pure PrimaryWildcard <* wildCardExpression

superclassExpression :: Parser SuperclassExpression
superclassExpression = do {kw "<superclass-expression>"; pure SuperclassExpression}

implicitMemberExpression :: Parser ()
implicitMemberExpression = do {kw "<implicit-member-expression>"; pure ()}

-- GRAMMAR OF A LITERAL EXPRESSION
literalExpression :: Parser LiteralExpression
literalExpression
    = RegularLiteral <$> literal
  <|> arrayLiteral
-- <|> dictionaryLiteral
  <|> SpecialLiteral <$> P.choice
        [ kw' "__FILE__"
        , kw' "__LINE__"
        , kw' "__COLUMN__"
        , kw' "__FUNCTION__"
        ]

arrayLiteral :: Parser LiteralExpression
arrayLiteral = ArrayLiteral <$> brackets (arrayLiteralItem `P.sepBy` comma)

arrayLiteralItem :: Parser Expression
arrayLiteralItem = expression

{-
dictionary-literal → [­dictionary-literal-items­]­  [­:­]­
dictionary-literal-items → dictionary-literal-item­,­opt­ dictionary-literal-item­,­dictionary-literal-items­
dictionary-literal-item → expression­:­expression­
-}

-- GRAMMAR OF A SELF EXPRESSION
selfExpression :: Parser SelfExpression
selfExpression = kw "self" *> P.choice
  [ try (pure Self2 <* tok "." <*> identifier)
  , try (pure Self3 <*> brackets expressionList)
  , try (pure Self4 <* tok "." <* kw "init")
  , try (pure Self1)
  ]

{-
GRAMMAR OF A SUPERCLASS EXPRESSION

superclass-expression → superclass-method-expression­  superclass-subscript-expression­ superclass-initializer-expression­
superclass-method-expression → super­.­identifier­
superclass-subscript-expression → super­[­expression-list­]­
superclass-initializer-expression → super­.­init­
-}

-- GRAMMAR OF A CLOSURE EXPRESSION

closureExpression :: Parser Closure
closureExpression = Closure <$> braces statements
{-
closure-expression → {­closure-signature­opt­statements­}­
closure-signature → parameter-clause­function-result­opt­in­
closure-signature → identifier-list­function-result­opt­in­
closure-signature → capture-list­parameter-clause­function-result­opt­in­
closure-signature → capture-list­identifier-list­function-result­opt­in­
closure-signature → capture-list­in­
capture-list → [­capture-list-items­]­
capture-list-items → capture-list-item­  capture-list-item­,­capture-list-items­
capture-list-item → capture-specifier­opt­expression­
capture-specifier → weak­  unowned­  unowned(safe)­  unowned(unsafe)­
GRAMMAR OF A IMPLICIT MEMBER EXPRESSION

implicit-member-expression → .­identifier­
-}

-- GRAMMAR OF A PARENTHESIZED EXPRESSION

parenthesizedExpression :: Parser [ExpressionElement]
parenthesizedExpression = P.choice
  [ try (parens (pure []))
  , parens expressionElementList
  ]

expressionElementList :: Parser [ExpressionElement]
expressionElementList = expressionElement `P.sepBy` comma

expressionElement :: Parser ExpressionElement
expressionElement
    = try (ExpressionElement <$> (Just <$> identifier) <* colon <*> expression)
  <|> ExpressionElement <$> pure Nothing <*> expression

-- GRAMMAR OF A WILDCARD EXPRESSION
wildCardExpression :: Parser ()
wildCardExpression = tok "_"

-- GRAMMAR OF A POSTFIX EXPRESSION

postfixExpression :: Parser PostfixExpression
postfixExpression = postfixExpressionOuter

notFollowedByPrimary :: Parser ()
notFollowedByPrimary = P.notFollowedBy primaryExpression

postfixExpressionOuter :: Parser PostfixExpression
postfixExpressionOuter = do
  e1 <- postfixExpressionInner
  e2 <- forcedValueExpressionTail e1 <* notFollowedByPrimary
    <|> optionalChainingExpressionTail e1 <* notFollowedByPrimary
    <|> postfixOpTail e1
    <|> dotTail e1
    <|> (FunctionCallE <$> functionCallTail e1)
    <|> Subscript <$> pure e1 <*> brackets expressionList
    <|> pure e1
  pure e2
    where
      postfixOpTail :: PostfixExpression -> Parser PostfixExpression
      postfixOpTail e = try $ do
        o <- operator
        notFollowedByPrimary
        return $ PostfixOperator e o
      dotTail :: PostfixExpression -> Parser PostfixExpression
      dotTail e = do
        _ <- tok "."
        postfixDynamicTypeTail e
          <|> postfixInitTail e
          <|> postfixSelfTail e
          <|> explicitMemberExpressionTail e

postfixExpressionInner :: Parser PostfixExpression
postfixExpressionInner = PostfixPrimary <$> primaryExpression

-- GRAMMAR OF A FUNCTION CALL EXPRESSION

functionCallTail :: PostfixExpression -> Parser FunctionCall
functionCallTail postfixE = do
  pe <- parenthesizedExpression
  c <- optional trailingClosure
  pure $ FunctionCall postfixE pe c

functionCallExpression :: Parser FunctionCall
functionCallExpression = do
  postfixE <- postfixExpression
  functionCallTail postfixE

trailingClosure :: Parser Closure
trailingClosure = closureExpression

-- GRAMMAR OF AN INITIALIZER EXPRESSION
-- We have already parsed postfixExpression and ".".
postfixInitTail :: PostfixExpression -> Parser PostfixExpression
postfixInitTail postfixE = kw "init" *> pure (PostfixExpression4Initalizer postfixE)

-- GRAMMAR OF AN EXPLICIT MEMBER EXPRESSION
-- We have already parsed postExpression and ".".
explicitMemberExpressionTail :: PostfixExpression -> Parser PostfixExpression
explicitMemberExpressionTail postfixE
    = try (ExplicitMemberExpressionDigits <$> pure postfixE <*> decimalDigits)
  <|> ExplicitMemberExpressionIdentifier <$> pure postfixE <*>
        (IdG <$> identifier <*> (fromMaybe [] <$> optional genericArgumentClause))

-- GRAMMAR OF A SELF EXPRESSION
-- We have already parsed postfixExpression and ".".
postfixSelfTail :: PostfixExpression -> Parser PostfixExpression
postfixSelfTail postfixE = kw "self" *> pure (PostfixSelf postfixE)

-- GRAMMAR OF A DYNAMIC TYPE EXPRESSION
postfixDynamicTypeTail :: PostfixExpression -> Parser PostfixExpression
postfixDynamicTypeTail postfixE = kw "dynamicType" *> pure (PostfixDynamicType postfixE)

-- GRAMMAR OF A SUBSCRIPT EXPRESSION
-- See Subscript above.

-- GRAMMAR OF A FORCED-VALUE EXPRESSION
forcedValueExpressionTail :: PostfixExpression -> Parser PostfixExpression
forcedValueExpressionTail e = op "!" *> pure (PostfixForcedValue e)

-- GRAMMAR OF AN OPTIONAL-CHAINING EXPRESSION
optionalChainingExpressionTail :: PostfixExpression -> Parser PostfixExpression
optionalChainingExpressionTail e = try $ do
  _ <- op "?"
  notFollowedByPrimary
  return $ PostfixOptionChaining e

{-
Lexical Structure

GRAMMAR OF AN IDENTIFIER

identifier → identifier-head­identifier-characters­opt­
identifier → `­identifier-head­identifier-characters­opt­`­
identifier → implicit-parameter-name­
identifier-list → identifier­  identifier­,­identifier-list­
identifier-head → Upper- or lowercase letter A through Z
identifier-head → _­
identifier-head → U+00A8, U+00AA, U+00AD, U+00AF, U+00B2–U+00B5, or U+00B7–U+00BA
identifier-head → U+00BC–U+00BE, U+00C0–U+00D6, U+00D8–U+00F6, or U+00F8–U+00FF
identifier-head → U+0100–U+02FF, U+0370–U+167F, U+1681–U+180D, or U+180F–U+1DBF
identifier-head → U+1E00–U+1FFF
identifier-head → U+200B–U+200D, U+202A–U+202E, U+203F–U+2040, U+2054, or U+2060–U+206F
identifier-head → U+2070–U+20CF, U+2100–U+218F, U+2460–U+24FF, or U+2776–U+2793
identifier-head → U+2C00–U+2DFF or U+2E80–U+2FFF
identifier-head → U+3004–U+3007, U+3021–U+302F, U+3031–U+303F, or U+3040–U+D7FF
identifier-head → U+F900–U+FD3D, U+FD40–U+FDCF, U+FDF0–U+FE1F, or U+FE30–U+FE44
identifier-head → U+FE47–U+FFFD
identifier-head → U+10000–U+1FFFD, U+20000–U+2FFFD, U+30000–U+3FFFD, or U+40000–U+4FFFD
identifier-head → U+50000–U+5FFFD, U+60000–U+6FFFD, U+70000–U+7FFFD, or U+80000–U+8FFFD
identifier-head → U+90000–U+9FFFD, U+A0000–U+AFFFD, U+B0000–U+BFFFD, or U+C0000–U+CFFFD
identifier-head → U+D0000–U+DFFFD or U+E0000–U+EFFFD
identifier-character → Digit 0 through 9
identifier-character → U+0300–U+036F, U+1DC0–U+1DFF, U+20D0–U+20FF, or U+FE20–U+FE2F
identifier-character → identifier-head­
identifier-characters → identifier-character­identifier-characters­opt­
implicit-parameter-name → $­decimal-digits­
-}

-- GRAMMAR OF A LITERAL
literal :: Parser Literal
literal = ws *>
  (try numericLiteral <|> try stringLiteral <|> try booleanLiteral <|> nilLiteral) <* ws

numericLiteral :: Parser Literal
numericLiteral = try optNegFloatingPointLiteral <|> optNegIntegerLiteral
  where
    optNegIntegerLiteral = do
      n <- optional (P.string "-")
      i <- integerLiteral
      return $ apply (neg n) i
    optNegFloatingPointLiteral = do
      n <- optional (P.string "-")
      f <- floatingPointLiteral
      return $ apply (neg n) f
    neg = maybe "" (const "-")
    apply n (NumericLiteral s) = NumericLiteral $ n ++ s

booleanLiteral :: Parser Literal
booleanLiteral = BooleanLiteral <$>
     (kw "true" *> pure True
  <|> kw "false" *> pure False)

nilLiteral :: Parser Literal
nilLiteral = pure NilLiteral <* kw "nil"

-- GRAMMAR OF AN INTEGER LITERAL
-- TODO use P.lookAhead here with case..of.
integerLiteral :: Parser Literal
integerLiteral
    = NumericLiteral <$>
         (try binaryLiteral
      <|> try octalLiteral
      <|> try hexLiteral
      <|> decimalLiteral)

binaryLiteral :: Parser String
binaryLiteral = do
  b <- P.string "0b"
  digits <- P.many1 binaryLiteralCharacter
  return $ b ++ digits
  where
    binaryDigit = P.oneOf ['0'..'1']
    binaryLiteralCharacter = binaryDigit <|> P.char '_'

octalLiteral :: Parser String
octalLiteral = do
  o <- P.string "0o"
  digits <- P.many1 octalLiteralCharacter
  return $ o ++ digits
  where
    octalDigit = P.oneOf ['0'..'7']
    octalLiteralCharacter = octalDigit <|> P.char '_'

decimalLiteral :: Parser String
decimalLiteral = P.many1 decimalLiteralCharacter
  where
    decimalLiteralCharacter = decimalDigit <|> P.char '_'

decimalDigit :: Parser Char
decimalDigit = P.oneOf ['0'..'9']

decimalDigits :: Parser String
decimalDigits = P.many1 decimalDigit

hexadecimalLiteral :: Parser String
hexadecimalLiteral = P.many1 hexLiteralCharacter
  where
    hexLiteralCharacter = hexDigit <|> P.char '_'

hexLiteral :: Parser String
hexLiteral = do
  h <- P.string "0x"
  digits <- P.many1 hexLiteralCharacter
  return $ h ++ digits
  where
    hexLiteralCharacter = hexDigit <|> P.char '_'

hexDigit :: Parser Char
hexDigit = P.oneOf (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])

stringyOptional :: Parser String -> Parser String
stringyOptional p = try p <|> pure ""

-- GRAMMAR OF A FLOATING-POINT LITERAL
floatingPointLiteral :: Parser Literal
floatingPointLiteral = try hexFloatingPoint <|> decFloatingPoint

decFloatingPoint :: Parser Literal
decFloatingPoint = do
  d <- decimalLiteral
  f <- stringyOptional decimalFraction
  e <- stringyOptional decimalExponent
  when (f == "" && e == "") $ fail "we want to parse an dec here"
  return $ NumericLiteral (d ++ f ++ e)

hexFloatingPoint :: Parser Literal
hexFloatingPoint = do
  h <- hexLiteral
  f <- stringyOptional hexFraction
  e <- stringyOptional hexExponent
  if f == "" && e == "" then fail "we want to parse a regular hex here" else pure ()
  return $ NumericLiteral (h ++ f ++ e)

-- floating-point-literal → hexadecimal-literal­hexadecimal-fraction­opt­hexadecimal-exponent­

decimalFraction :: Parser String
decimalFraction = do
  dot <- tok' "."
  dec <- decimalLiteral
  return $ dot ++ dec

decimalExponent :: Parser String
decimalExponent = do
  e <- floatingPointE
  s <- stringyOptional sign
  dec <- decimalLiteral
  return $ e ++ s ++ dec

hexFraction :: Parser String
hexFraction = do
  dot <- tok' "."
  hex <- hexadecimalLiteral
  return $ dot ++ hex

hexExponent :: Parser String
hexExponent = do
  e <- floatingPointP
  s <- stringyOptional sign
  hex <- decimalLiteral
  return $ e ++ s ++ hex

floatingPointE :: Parser String
floatingPointE = (: []) <$> P.oneOf "eE"

floatingPointP :: Parser String
floatingPointP = (: []) <$> P.oneOf "pP"

sign :: Parser String
sign = (: []) <$> P.oneOf "+-"

-- GRAMMAR OF A STRING LITERAL
stringLiteral :: Parser Literal
stringLiteral = StringLiteral <$> (try staticStringLiteral <|> interpolatedStringLiteral)

staticStringLiteral :: Parser StringLiteral
staticStringLiteral = StaticStringLiteral <$> do
  _ <- P.char '"'
  is <- P.many quotedTextItem
  _ <- P.char '"'
  return $ Prelude.concat is

quotedTextItem :: Parser String
quotedTextItem
    = escapedCharacter
  <|> P.many1 (P.noneOf "\"\\\x000A\x000D") -- any Unicode scalar value except " (double-quote), \ (backslash), U+000A, or U+000D

interpolatedStringLiteral :: Parser StringLiteral
interpolatedStringLiteral = InterpolatedStringLiteral <$> do
  _ <- P.char '"'
  tis <- P.many interpolatedTextItem
  _ <- P.char '"'
  return tis

interpolatedTextItem :: Parser InterpolatedTextItem
interpolatedTextItem
    = try $ TextItemExpr <$> do
        _ <- P.string "\\("
        e <- expression
        _ <- P.string ")"
        return e
    <|> TextItemString <$> quotedTextItem

escapedCharacter :: Parser String
escapedCharacter
    = (try . P.string) "\\0" *> pure "\0"
  <|> (try . P.string) "\\\\" *> pure "\\"
  <|> (try . P.string) "\\t" *> pure "\t"
  <|> (try . P.string) "\\n" *> pure "\n"
  <|> (try . P.string) "\\r" *> pure "\r"
  <|> (try . P.string) "\\\"" *> pure "\""
  <|> (try . P.string) "\\'" *> pure "'"
  <|> do
        u <- P.string "\\u"
        d <- unicodeScalarDigits
        return $ u ++ d

-- 1..8 hex digits
unicodeScalarDigits :: Parser String
unicodeScalarDigits = P.choice
  [ P.count 8 hexDigit
  , P.count 7 hexDigit
  , P.count 6 hexDigit
  , P.count 5 hexDigit
  , P.count 4 hexDigit
  , P.count 3 hexDigit
  , P.count 2 hexDigit
  , P.count 1 hexDigit
  ]

-- GRAMMAR OF OPERATORS

operator :: Parser String
operator
    = try $ do
        _ <- ws
        h <- operatorHead
        cs <- operatorCharacters
        _ <- ws
        return (h : cs)
  <|> do
        _ <- ws
        _ <- P.char '`'
        h <- operatorHead
        cs <- operatorCharacters
        _ <- P.char '`'
        _ <- ws
        return (h : cs)

-- XXX removing '=' for now...
legalHeadOperatorChars :: String
legalHeadOperatorChars =
  "=/-+!*%<>&|^~?"
  ++ ['\x00A1' .. '\x00A7']
  ++ "\x00A9\x00AB"
  ++ "\x00AC\x00AE"
  ++ ['\x00B0'..'\x00B1'] ++ "\x00B6\x00BB\x00BF\x00D7\x00F7"
  ++ ['\x2016'..'\x2017'] ++ ['\x2020'..'\x2027']
  ++ ['\x2030'..'\x203E']
  ++ ['\x2041'..'\x2053']
  ++ ['\x2055'..'\x205E']
  ++ ['\x2190'..'\x23FF']
  ++ ['\x2500'..'\x2775']
  ++ ['\x2794'..'\x2BFF']
  ++ ['\x2E00'..'\x2E7F']
  ++ ['\x3001'..'\x3003']
  ++ ['\x3008'..'\x3030']

legalTailOperatorChars :: String
legalTailOperatorChars = legalHeadOperatorChars
  ++ ['\x0300'..'\x036F']
  ++ ['\x1DC0'..'\x1DFF']
  ++ ['\x20D0'..'\x20FF']
  ++ ['\xFE00'..'\xFE0F']
  ++ ['\xFE20'..'\xFE2F']
  ++ ['\xE0100'..'\xE01FF']

operatorHead :: Parser Char
operatorHead = P.oneOf legalHeadOperatorChars

operatorCharacter :: Parser Char
operatorCharacter = P.oneOf legalTailOperatorChars

operatorCharacters :: Parser String
operatorCharacters = P.many operatorCharacter

-- dotOperatorHead = P.string ".."
-- dotOperatorCharacter = P.string "." <|> operatorCharacter
-- dotOperatorCharacters = P.many dotOperatorCharacter

binaryOperator :: Parser String
binaryOperator = operator

prefixOperator :: Parser String
prefixOperator = operator

postfixOperator :: Parser String
postfixOperator = operator

------------------------------------------------------------
-- Types
------------------------------------------------------------

-- GRAMMAR OF A TYPE
type_ :: Parser Type
type_ = do
  t <- typeInit
  (optionalTypeTail t <|> implicitlyUnwrappedOptionalTypeTail t) <|> pure t

typeInit :: Parser Type
typeInit = SimpleType <$> identifier
-- type → array-type­  dictionary-type­  function-type­  type-identifier­  tuple-type­  optional-type­  implicitly-unwrapped-optional-type­  protocol-composition-type­  metatype-type­

-- GRAMMAR OF A TYPE ANNOTATION
typeAnnotation :: Parser TypeAnnotation
typeAnnotation = tok ":" *> (TypeAnnotation <$> attributeList <*> type_)

-- GRAMMAR OF A TYPE IDENTIFIER
-- type-identifier → type-name­generic-argument-clause­opt­  type-name­generic-argument-clause­opt­.­type-identifier­
-- type-name → identifier­

-- GRAMMAR OF A TUPLE TYPE
-- tuple-type → (­tuple-type-body­opt­)­
-- tuple-type-body → tuple-type-element-list­...­opt­
-- tuple-type-element-list → tuple-type-element­  tuple-type-element­,­tuple-type-element-list­
-- tuple-type-element → attributes­opt­inout­opt­type­  inout­opt­element-name­type-annotation­
-- element-name → identifier­

-- GRAMMAR OF A FUNCTION TYPE
-- function-type → type­throws­opt­->­type­
-- function-type → type­rethrows­->­type­

-- GRAMMAR OF AN ARRAY TYPE
-- array-type → [­type­]­

-- GRAMMAR OF A DICTIONARY TYPE
-- dictionary-type → [­type­:­type­]­

-- GRAMMAR OF AN OPTIONAL TYPE
optionalTypeTail :: Type -> Parser Type
optionalTypeTail t = op "?" *> pure (TypeOpt t)

-- GRAMMAR OF AN IMPLICITLY UNWRAPPED OPTIONAL TYPE
implicitlyUnwrappedOptionalTypeTail :: Type -> Parser Type
implicitlyUnwrappedOptionalTypeTail t = op "!" *> pure (ImplicitlyUnwrappedOptType t)

-- GRAMMAR OF A PROTOCOL COMPOSITION TYPE
-- protocol-composition-type → protocol­<­protocol-identifier-list­opt­>­
-- protocol-identifier-list → protocol-identifier­  protocol-identifier­,­protocol-identifier-list­
-- protocol-identifier → type-identifier­

-- GRAMMAR OF A METATYPE TYPE
-- metatype-type → type­.­Type­  type­.­Protocol­

-- GRAMMAR OF A TYPE INHERITANCE CLAUSE
-- type-inheritance-clause → :­class-requirement­,­type-inheritance-list­
-- type-inheritance-clause → :­class-requirement­
-- type-inheritance-clause → :­type-inheritance-list­
-- type-inheritance-list → type-identifier­  type-identifier­,­type-inheritance-list­

-- class-requirement → class­
