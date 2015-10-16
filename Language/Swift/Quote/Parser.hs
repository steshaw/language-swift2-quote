{-# LANGUAGE FlexibleContexts #-}

module Language.Swift.Quote.Parser where

import Language.Swift.Quote.Syntax

import Control.Applicative
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
braces = T.braces lexer

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
  -- <|> branchStatement <* optSemicolon
  -- <|> labeledStatement <* optSemicolon
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
          -- _ <- trace ("i = " ++ show i) $ pure ()
          sc1 <- semicolon
          -- _ <- trace ("sc1 = " ++ show sc1) $ pure ()
          e1 <- optional expression
          -- _ <- trace ("e1 = " ++ show e1) $ pure ()
          sc2 <- semicolon
          -- _ <- trace ("sc2 = " ++ show sc2) $ pure ()
          e2 <- optional expression
          -- _ <- trace ("e2 = " ++ show e2) $ pure ()
          s3 <- try . P.lookAhead $ many P.anyChar
          -- _ <- trace ("any s = " ++ show s3) $ pure ()
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

{-
GRAMMAR OF A BRANCH STATEMENT

branch-statement → if-statement­
branch-statement → guard-statement­
branch-statement → switch-statement­
GRAMMAR OF AN IF STATEMENT

if-statement → if­condition-clause­code-block­else-clause­opt­
else-clause → else­code-block­  else­if-statement­
GRAMMAR OF A GUARD STATEMENT

guard-statement → guard­condition-clause­else­code-block­
GRAMMAR OF A SWITCH STATEMENT

switch-statement → switch­expression­{­switch-cases­opt­}­
switch-cases → switch-case­switch-cases­opt­
switch-case → case-label­statements­  default-label­statements­
case-label → case­case-item-list­:­
case-item-list → pattern­where-clause­opt­  pattern­where-clause­opt­,­case-item-list­
default-label → default­:­
where-clause → where­where-expression­
where-expression → expression­
GRAMMAR OF A LABELED STATEMENT

labeled-statement → statement-label­loop-statement­  statement-label­if-statement­ statement-label­switch-statement­
statement-label → label-name­:­
label-name → identifier­

-}

-- GRAMMAR OF A CONTROL TRANSFER STATEMENT

controlTransferStatement :: Parser Statement
controlTransferStatement = returnStatement

-- control-transfer-statement → break-statement­
-- control-transfer-statement → continue-statement­
-- control-transfer-statement → fallthrough-statement­
-- control-transfer-statement → return-statement­
-- control-transfer-statement → throw-statement­

{-
GRAMMAR OF A BREAK STATEMENT

break-statement → break­label-name­opt­
GRAMMAR OF A CONTINUE STATEMENT

continue-statement → continue­label-name­opt­
GRAMMAR OF A FALLTHROUGH STATEMENT

fallthrough-statement → fallthrough­
-}

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
  <|> constantDeclaration
  <|> variableDeclaration
  <|> typealiasDeclaration
  <|> functionDeclaration
{-
declaration → enum-declaration­
declaration → struct-declaration­
declaration → class-declaration­
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
  = variableDeclarationHead *> (SimpleVariableDeclaration <$> patternInitializerList)

variableDeclarationHead :: Parser ([Attribute], [DeclarationModifier])
variableDeclarationHead = do
  atts <- attributeList
  mods <- fromMaybe [] <$> optional declarationModifiers
  _ <- kw "var"
  return (atts, mods)

-- variable-declaration → variable-declaration-head­variable-name­type-annotation­code-block­
-- variable-declaration → variable-declaration-head­variable-name­type-annotation­getter-setter-block­
-- variable-declaration → variable-declaration-head­variable-name­type-annotation­getter-setter-keyword-block­
-- variable-declaration → variable-declaration-head­variable-name­initializer­willSet-didSet-block­
-- variable-declaration → variable-declaration-head­variable-name­type-annotation­initializer­opt­willSet-didSet-block­
-- variable-name → identifier­
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

{-
GRAMMAR OF AN ENUMERATION DECLARATION

enum-declaration → attributes­opt­access-level-modifier­opt­union-style-enum­
enum-declaration → attributes­opt­access-level-modifier­opt­raw-value-style-enum­
union-style-enum → indirect­opt­enum­enum-name­generic-parameter-clause­opt­type-inheritance-clause­opt­{­union-style-enum-members­opt­}­
union-style-enum-members → union-style-enum-member­union-style-enum-members­opt­
union-style-enum-member → declaration­  union-style-enum-case-clause­
union-style-enum-case-clause → attributes­opt­indirect­opt­case­union-style-enum-case-list­
union-style-enum-case-list → union-style-enum-case­ union-style-enum-case­,­union-style-enum-case-list­
union-style-enum-case → enum-case-name­tuple-type­opt­
enum-name → identifier­
enum-case-name → identifier­
raw-value-style-enum → enum­enum-name­generic-parameter-clause­opt­type-inheritance-clause­{­raw-value-style-enum-members­}­
raw-value-style-enum-members → raw-value-style-enum-member­raw-value-style-enum-members­opt­
raw-value-style-enum-member → declaration­  raw-value-style-enum-case-clause­
raw-value-style-enum-case-clause → attributes­opt­case­raw-value-style-enum-case-list­
raw-value-style-enum-case-list → raw-value-style-enum-case­ raw-value-style-enum-case­,­raw-value-style-enum-case-list­
raw-value-style-enum-case → enum-case-name­raw-value-assignment­opt­
raw-value-assignment → =­raw-value-literal­
raw-value-literal → numeric-literal­  static-string-literal­  boolean-literal­
GRAMMAR OF A STRUCTURE DECLARATION

struct-declaration → attributes­opt­access-level-modifier­opt­struct­struct-name­generic-parameter-clause­opt­type-inheritance-clause­opt­struct-body­
struct-name → identifier­
struct-body → {­declarations­opt­}­
GRAMMAR OF A CLASS DECLARATION

class-declaration → attributes­opt­access-level-modifier­opt­class­class-name­generic-parameter-clause­opt­type-inheritance-clause­opt­class-body­
class-name → identifier­
class-body → {­declarations­opt­}­
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
pattern = ExpressionPattern <$> expression
-- pattern → wildcard-pattern­type-annotation­opt­
-- pattern → identifier-pattern­type-annotation­opt­
-- pattern → value-binding-pattern­
-- pattern → tuple-pattern­type-annotation­opt­
-- pattern → enum-case-pattern­
-- pattern → optional-pattern­
-- pattern → type-casting-pattern­
-- pattern → expression-pattern­

{-
GRAMMAR OF A WILDCARD PATTERN

wildcard-pattern → _­
GRAMMAR OF AN IDENTIFIER PATTERN

identifier-pattern → identifier­
GRAMMAR OF A VALUE-BINDING PATTERN

value-binding-pattern → var­pattern­  let­pattern­
GRAMMAR OF A TUPLE PATTERN

tuple-pattern → (­tuple-pattern-element-list­opt­)­
tuple-pattern-element-list → tuple-pattern-element­ tuple-pattern-element­,­tuple-pattern-element-list­
tuple-pattern-element → pattern­
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

newBinary :: Parser BinaryExpression
newBinary = do
  o <- binaryOperator
  -- o <- op' "*"
  lit <- numericLiteral
  return $ BinaryExpression1 o (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral lit))))

binaryExpression :: Parser BinaryExpression
binaryExpression
    = do
        _ <- assignmentOperator
        to <- optional tryOperator
        pe <- prefixExpression
        return $ BinaryAssignmentExpression to pe
  <|> do
        -- _ <- trace "\n\n\nin 2nd case" $ pure ()
        o <- binaryOperator
        -- _ <- trace ("\nbinaryOperator = " ++ show o) $ pure ()
        s2 <- try . P.lookAhead $ many P.anyChar
        -- _ <- trace ("\ntrying for prefixExpression in binaryExpression lookAhead s2 = " ++ show s2) $ pure ()
        e <- prefixExpression
        -- _ <- trace ("\nprefix = " ++ show e) $ pure ()
        return $ BinaryExpression1 o e
  <|> do
        co <- conditionalOperator
        to <- optional tryOperator
        pe <- prefixExpression
        return $ BinaryExpression3 co to pe
  <|> typeCastingOperator

binaryExpressions :: Parser [BinaryExpression]
binaryExpressions = many binaryExpression

-- GRAMMAR OF AN ASSIGNMENT OPERATOR
assignmentOperator :: Parser ()
assignmentOperator = tok "="

-- GRAMMAR OF A CONDITIONAL OPERATOR
conditionalOperator :: Parser (Maybe String, Expression)
conditionalOperator = do
  _ <- op "?"
  to <- optional tryOperator
  e <- expression
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
-- <|> literalExpression = arrayLiteral <|> dictionaryLiteral
  <|> SpecialLiteral <$> P.choice
        [ kw' "__FILE__"
        , kw' "__LINE__"
        , kw' "__COLUMN__"
        , kw' "__FUNCTION__"
        ]

{-
array-literal → [­array-literal-items­opt­]­
array-literal-items → array-literal-item­,­opt­  array-literal-item­,­array-literal-items­
array-literal-item → expression­
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

postfixExpressionOuter :: Parser PostfixExpression
postfixExpressionOuter = do
  e1 <- postfixExpressionInner
  e2 <- (op "!" *> pure (PostfixForcedValue e1))
        <|> (op "?" *> pure (PostfixOptionChaining e1))
        -- <|> postfixOpTail e1 -- FIXME Prevents binary expressions being recognised.
        <|> dotTail e1
        <|> (FunctionCallE <$> functionCallTail e1)
        <|> Subscript <$> pure e1 <*> brackets expressionList
        <|> pure e1
  pure e2
    where
      postfixOpTail :: PostfixExpression -> Parser PostfixExpression
      postfixOpTail e = PostfixOperator <$> pure e <*> operator
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

--GRAMMAR OF A FORCED-VALUE EXPRESSION
-- See PostfixForcedValue

-- GRAMMAR OF AN OPTIONAL-CHAINING EXPRESSION
-- See PostfixOptionChaining

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
  (numericLiteral <|> stringLiteral <|> booleanLiteral <|> nilLiteral)

-- numeric-literal → - ­opt ­integer-literal |­ - ­opt ­floating-point-literal­
{-
numericLiteral
    = optional (op "-") integerLiteral
  <|> optional (op "-") floatingPointLiteral
-}
numericLiteral :: Parser Literal
numericLiteral = try floatingPointLiteral <|> integerLiteral

-- GRAMMAR OF AN INTEGER LITERAL

decimalDigits :: Parser String
decimalDigits = P.many1 P.digit

{-
integer-literal → binary-literal­
integer-literal → octal-literal­
integer-literal → decimal-literal­
integer-literal → hexadecimal-literal­
binary-literal → 0b­binary-digit­binary-literal-characters­opt­
binary-digit → Digit 0 or 1
binary-literal-character → binary-digit­  _­
binary-literal-characters → binary-literal-character­binary-literal-characters­opt­
octal-literal → 0o­octal-digit­octal-literal-characters­opt­
octal-digit → Digit 0 through 7
octal-literal-character → octal-digit­  _­
octal-literal-characters → octal-literal-character­octal-literal-characters­opt­
decimal-literal → decimal-digit­decimal-literal-characters­opt­
decimal-digit → Digit 0 through 9
decimal-digits → decimal-digit­decimal-digits­opt­
decimal-literal-character → decimal-digit­  _­
decimal-literal-characters → decimal-literal-character­decimal-literal-characters­opt­
hexadecimal-literal → 0x­hexadecimal-digit­hexadecimal-literal-characters­opt­
hexadecimal-digit → Digit 0 through 9, a through f, or A through F
hexadecimal-literal-character → hexadecimal-digit­  _­
hexadecimal-literal-characters → hexadecimal-literal-character­hexadecimal-literal-characters­opt­
-}
-- TODO: simplified
integerLiteral :: Parser Literal
integerLiteral = IntegerLiteral <$> T.integer lexer

-- GRAMMAR OF A FLOATING-POINT LITERAL
{-
floating-point-literal → decimal-literal­decimal-fraction­opt­decimal-exponent­opt­
floating-point-literal → hexadecimal-literal­hexadecimal-fraction­opt­hexadecimal-exponent­
decimal-fraction → .­decimal-literal­
decimal-exponent → floating-point-e­sign­opt­decimal-literal­
hexadecimal-fraction → .­hexadecimal-digit­hexadecimal-literal-characters­opt­
hexadecimal-exponent → floating-point-p­sign­opt­decimal-literal­
floating-point-e → e­  E­
floating-point-p → p­  P­
sign → +­  -­
-}
-- TODO: simplified
floatingPointLiteral :: Parser Literal
floatingPointLiteral = FloatingPointLiteral <$> T.float lexer

--GRAMMAR OF A STRING LITERAL
{-
string-literal → static-string-literal­  interpolated-string-literal­
static-string-literal → "­quoted-text­opt­"­
quoted-text → quoted-text-item­quoted-text­opt­
quoted-text-item → escaped-character­
quoted-text-item → Any Unicode scalar value except "­, \­, U+000A, or U+000D
interpolated-string-literal → "­interpolated-text­opt­"­
interpolated-text → interpolated-text-item­interpolated-text­opt­
interpolated-text-item → \(­expression­)­  quoted-text-item­
escaped-character → \0­  \\­  \t­  \n­  \r­  \"­  \'­
escaped-character → \u­{­unicode-scalar-digits­}­
unicode-scalar-digits → Between one and eight hexadecimal digits
-}
-- TODO: simplified
stringLiteral :: Parser Literal
stringLiteral = StringLiteral <$> T.stringLiteral lexer

booleanLiteral :: Parser Literal
booleanLiteral = BooleanLiteral <$>
     (kw "true" *> pure True
  <|> kw "false" *> pure False)

nilLiteral :: Parser Literal
nilLiteral = pure NilLiteral <* kw "nil"

-- GRAMMAR OF OPERATORS
-- TODO: simplified!
-- operator :: Parser String
-- operator = T.reservedOp lexer

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
  "/-+!*%<>&|^~?"
  -- ++ ['\x00A1' .. '\x00A7']
  -- ++ "\x00A9\x00AB"
  -- ++ "\x00AC\x00AE"
  -- ++ ['\x00B0'..'\x00B1'] ++ "\x00B6\x00BB\x00BF\x00D7\x00F7"
  -- ++ ['\x2016'..'\x2017'] ++ ['\x2020'..'\x2027']
  -- ++ ['\x2030'..'\x203E']
  -- ++ ['\x2041'..'\x2053']
-- operator-head → U+2055–U+205E
-- operator-head → U+2190–U+23FF
-- operator-head → U+2500–U+2775
-- operator-head → U+2794–U+2BFF
-- operator-head → U+2E00–U+2E7F
-- operator-head → U+3001–U+3003
-- operator-head → U+3008–U+3030

legalTailOperatorChars :: String
legalTailOperatorChars = legalHeadOperatorChars
-- operator-character → U+0300–U+036F
-- operator-character → U+1DC0–U+1DFF
-- operator-character → U+20D0–U+20FF
-- operator-character → U+FE00–U+FE0F
-- operator-character → U+FE20–U+FE2F
-- operator-character → U+E0100–U+E01EF

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
type_ = Type <$> identifier
-- type → array-type­  dictionary-type­  function-type­  type-identifier­  tuple-type­  optional-type­  implicitly-unwrapped-optional-type­  protocol-composition-type­  metatype-type­

-- GRAMMAR OF A TYPE ANNOTATION
typeAnnotation :: Parser TypeAnnotation
typeAnnotation = tok ":" *> (TypeAnnotation <$> attributeList <*> type_)

{-
GRAMMAR OF A TYPE IDENTIFIER

type-identifier → type-name­generic-argument-clause­opt­  type-name­generic-argument-clause­opt­.­type-identifier­
type-name → identifier­
GRAMMAR OF A TUPLE TYPE

tuple-type → (­tuple-type-body­opt­)­
tuple-type-body → tuple-type-element-list­...­opt­
tuple-type-element-list → tuple-type-element­  tuple-type-element­,­tuple-type-element-list­
tuple-type-element → attributes­opt­inout­opt­type­  inout­opt­element-name­type-annotation­
element-name → identifier­
GRAMMAR OF A FUNCTION TYPE

function-type → type­throws­opt­->­type­
function-type → type­rethrows­->­type­
GRAMMAR OF AN ARRAY TYPE

array-type → [­type­]­
GRAMMAR OF A DICTIONARY TYPE

dictionary-type → [­type­:­type­]­
GRAMMAR OF AN OPTIONAL TYPE

optional-type → type­?­
GRAMMAR OF AN IMPLICITLY UNWRAPPED OPTIONAL TYPE

implicitly-unwrapped-optional-type → type­!­
GRAMMAR OF A PROTOCOL COMPOSITION TYPE

protocol-composition-type → protocol­<­protocol-identifier-list­opt­>­
protocol-identifier-list → protocol-identifier­  protocol-identifier­,­protocol-identifier-list­
protocol-identifier → type-identifier­
GRAMMAR OF A METATYPE TYPE

metatype-type → type­.­Type­  type­.­Protocol­
GRAMMAR OF A TYPE INHERITANCE CLAUSE

type-inheritance-clause → :­class-requirement­,­type-inheritance-list­
type-inheritance-clause → :­class-requirement­
type-inheritance-clause → :­type-inheritance-list­
type-inheritance-list → type-identifier­  type-identifier­,­type-inheritance-list­
class-requirement → class­
-}
