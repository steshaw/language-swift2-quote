{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Swift.Quote.Pretty where

import Language.Swift.Quote.Syntax

import Data.Text.Lazy (Text, append)
import Data.Maybe
import Text.PrettyPrint.Mainland

------------------------------------------------------------
-- Auxiliary
------------------------------------------------------------
bracesLines :: [Doc] -> Doc
bracesLines [] = lbrace <> line <> rbrace
bracesLines ds
  = lbrace <> line
  <> ind (stack ds)
  <> line <> rbrace

prettyPrint :: Module -> Text
prettyPrint m = append p "\n"
  where p = prettyLazyText 100 (ppr m)

ind :: Doc -> Doc
ind = indent 2

sepBySpace :: Pretty p => [p] -> Doc
sepBySpace [] = empty
sepBySpace p = sep (map ppr p)

ppExps :: [Expression] -> Doc
ppExps expressions = commasep (map ppr expressions)

ppBracketExps :: [Expression] -> Doc
ppBracketExps = brackets . ppExps

ppExpressionElements :: [ExpressionElement] -> Doc
ppExpressionElements = parens . commasep . map ppr

------------------------------------------------------------
-- Pretty instances
------------------------------------------------------------

instance Pretty Module where
  ppr (Module statements) = stack (map ppr statements)

instance Pretty Expression where
  ppr (Expression optTryOperator prefixExpression binaryExpressions) =
    ppr optTryOperator <+> ppr prefixExpression <+> spread (map ppr binaryExpressions)

instance Pretty PrefixExpression where
  ppr (PrefixExpression optPrefixExpression primaryExpression)
    = ppr optPrefixExpression <> ppr primaryExpression
  ppr (InOutExpression identifier) = string "&" <> string identifier

instance Pretty PostfixExpression where
  ppr (PostfixPrimary primaryExpression) = ppr primaryExpression
  ppr (PostfixOperator prefixExpression postfixOperator) = ppr prefixExpression <> ppr postfixOperator
  ppr (ExplicitMemberExpressionDigits postfixExpression digits) = ppr postfixExpression <> dot <> string digits
  ppr (ExplicitMemberExpressionIdentifier postfixExpression idG) = ppr postfixExpression <> dot <> ppr idG
  ppr (FunctionCallE functionCall) = ppr functionCall
  ppr (PostfixExpression4Initalizer postfixExpression) = ppr postfixExpression <> string ".init"
  ppr (PostfixSelf postfixExpression) = ppr postfixExpression <> string ".self"
  ppr (PostfixDynamicType postfixExpression) = ppr postfixExpression <> string ".dynamicType"
  ppr (PostfixForcedValue postfixExpression) = ppr postfixExpression <> string "!"
  ppr (PostfixOptionChaining postfixExpression) = ppr postfixExpression <> string "?"
  ppr (Subscript postfixExpression expressions) = ppr postfixExpression <> ppBracketExps expressions

instance Pretty FunctionCall where
  ppr (FunctionCall postfixExpression expressionElements optClosure) =
    ppr postfixExpression
      <> ppExpressionElements expressionElements
      <> ppr optClosure

instance Pretty ExpressionElement where
  ppr (ExpressionElement Nothing expression) = ppr expression
  ppr (ExpressionElement (Just ident) expression) = ppr ident <> colon <+> ppr expression

instance Pretty Closure where
  ppr (Closure []) = empty
  ppr (Closure statements) = braces (ppr statements)

instance Pretty PrimaryExpression where
  ppr (PrimaryExpression1 idG) = ppr idG
  ppr (PrimaryLiteral literalExpression) = ppr literalExpression
  ppr (PrimarySelf selfExpression) = ppr selfExpression
  ppr (PrimarySuper superclassExpression) = ppr superclassExpression
  ppr (PrimaryClosure closure) = ppr closure
  ppr (PrimaryParenthesized expressionElements) = (parens . commasep .  map ppr) expressionElements
  ppr PrimaryImplicitMember = string "<implicit-member-expression>" -- TODO implicit-member-expression
  ppr PrimaryWildcard = string "_"

instance Pretty IdG where
  ppr (IdG identifier []) = ppr identifier
  ppr (IdG identifier genericArgs) = ppr identifier <> angles (ppr genericArgs)

instance Pretty BinaryExpression where
  ppr (BinaryExpression1 operator prefixExpression) = ppr operator <+> ppr prefixExpression
  ppr (BinaryAssignmentExpression tryOperator prefixExpression) = string "=" <+> ppr tryOperator <+> ppr prefixExpression
  ppr (BinaryConditional (optTry1, expression) optTry2 prefixExpression) =
    string "?" <+> ppr optTry1 <+> ppr expression <+> string ":" <+> ppr optTry2 <+> ppr prefixExpression
  ppr (BinaryExpression4 s typ) = ppr s <> ppr typ

instance Pretty LiteralExpression where
  ppr (RegularLiteral lit) =  ppr lit
  ppr (SpecialLiteral special) =  ppr special
  ppr (ArrayLiteral items) = (brackets . commasep) (map ppr items)
  ppr (DictionaryLiteral items)
    = (brackets . commasep) (map pprTuple items)
      where
        pprTuple (e1, e2) = ppr e1 <+> string ":" <+> ppr e2

instance Pretty Literal where
  ppr (NumericLiteral s) = string s
  ppr (StringLiteral s) = ppr s
  ppr (BooleanLiteral b) = if b then text "true" else text "false"
  ppr NilLiteral = text "nil"

instance Pretty StringLiteral where
  ppr (StaticStringLiteral s) = dquotes (string s)
  ppr (InterpolatedStringLiteral items) = (dquotes . cat) (map ppr items)

instance Pretty InterpolatedTextItem where
  ppr (TextItemString s) = string s
  ppr (TextItemExpr e) = string "\\(" <> ppr e <> string ")"

instance Pretty SelfExpression where
  ppr Self = string "self"
  ppr (SelfMethod identifier) = string "self" <> string "." <> string identifier
  ppr (SelfSubscript expressions) = string "self" <> ppBracketExps expressions
  ppr SelfInit = string "self" <> string "." <> string "init"

instance Pretty SuperclassExpression where
  ppr (SuperMethod identifier) = string "super" <> string "." <> string identifier
  ppr (SuperSubscript expressions) = string "super" <> ppBracketExps expressions
  ppr SuperInit = string "super" <> string "." <> string "init"

instance Pretty Type where
  ppr (SimpleType ty) = ppr ty
  ppr (TypeOpt ty) = ppr ty <> string "?"
  ppr (ImplicitlyUnwrappedOptType ty) = ppr ty <> string "!"

instance Pretty Statement where
  ppr (ExpressionStatement expression) = ppr expression
  ppr (WhileStatement expression block) = string "while" <+> ppr expression <+> ppr block
  ppr (RepeatWhileStatement block expression) = string "repeat" <+> ppr block <+> string "while" <+> ppr expression
  ppr (ForStatement iE cE nE block) = string "for" <+> ppr iE <> semi
                                           <+> ppr cE <> semi <+> ppr nE <+> ppr block
  ppr (DeclarationStatement declaration) = ppr declaration
  ppr (ReturnStatement optExpression) = string "return" <+> ppr optExpression
  ppr (IfStatement cond ifBlock Nothing)
    = string "if" <+> ppr cond <+> ppr ifBlock
  ppr (IfStatement cond ifBlock (Just eitherBlockIf))
    = string "if" <+> ppr cond <+> ppr ifBlock <+> string "else" <+> ppBoth eitherBlockIf
      where
        ppBoth (Left block) = ppr block
        ppBoth (Right ifStatement) = ppr ifStatement

instance Pretty ForInit where
  ppr (FiDeclaration declaration) = ppr declaration
  ppr (FiExpressionList expressions) = ppExps expressions

instance Pretty Declaration where
  ppr (ImportDeclaration attributes optImportKind importPath)
      = sepBySpace attributes
      <+> string "import"
      <+> ppr optImportKind
      <+> (cat . punctuate dot) (map ppr importPath)

  ppr (DeclVariableDeclaration variableDeclaration) = ppr variableDeclaration

  ppr (ConstantDeclaration attributes declarationModifiers patternInitialisers)
      = sepBySpace attributes
    <+> sepBySpace declarationModifiers
    <+> string "let"
    <+> commasep (map ppr patternInitialisers)

  ppr (TypeAlias attributes optDeclarationModifier name typ_)
      = string "typealias"
    <+> sepBySpace attributes
    <+> ppr optDeclarationModifier
    <+> string name <+> string "=" <+> ppr typ_ -- TODO

  ppr (FunctionDeclaration attrs decls name optGenericParamClause parameterClauses optThrowDecl optResult optBlock)
      = sepBySpace attrs
    <+> sepBySpace decls
    <+> string "func"
    <+> ppr name
-- TODO
    -- <+> ppr optGenericParamClause
    <> sep (map (parens . commasep . map ppr) parameterClauses)
    <+> ppr optResult
    <+> ppr optBlock

  ppr (EnumDeclaration enum) = ppr enum
  ppr (StructDeclaration ty atts optMod name optGPC optTIC decls) = ppr ty <+> string name <+> bracesLines (map ppr decls)

instance Pretty StructType where
  ppr Struct = string "struct"
  ppr Class = string "class"

instance Pretty EnumDeclaration where
  ppr (UnionEnum atts optMod isIndirect name optGPC optTIC members)
      = string "enum" <+> string name
    <+> bracesLines (map ppr members)-- TODO

instance Pretty UnionStyleEnumMember where
    ppr (EnumMemberDeclaration d) = ppr d
    ppr (EnumMemberCase atts isIndirect cases) = string "case" <+> commasep (map printCase cases)
      where
        printCase :: (CaseName, Maybe {-Tuple-} Type) -> Doc
        printCase (n, t) = string n <+> ppr t

instance Pretty FunctionName where
  ppr (FunctionNameIdent s) = string s
  ppr (FunctionNameOp s) = string s

instance Pretty FunctionResult where
  ppr (FunctionResult attrs type_) = string "->" <+> sepBySpace attrs <+> ppr type_

instance Pretty Parameter where
  ppr (ParameterLet optExternName localName typeAnnotation optExpression)
    = string localName
    -- <> string ":"
    <> ppr typeAnnotation
  -- | ParameterVar (Maybe String) String TypeAnnotation (Maybe Expression)
  -- | ParameterInOut (Maybe String) String TypeAnnotation
  -- | ParameterDots (Maybe String) String TypeAnnotation

instance Pretty Attribute where
  ppr = undefined

instance Pretty DeclarationModifier where
  ppr (Modifier s) = ppr s
  ppr (AccessLevelModifier s) = ppr s

instance Pretty ImportPathIdentifier where
  ppr (ImportIdentifier s) = ppr s
  ppr (ImportOperator s) = ppr s

instance Pretty VariableDeclaration where
  ppr (VarPatternInitializer attrs mods patternInitialisers) = string "var" <+> commasep (map ppr patternInitialisers)
  ppr (VarSimple attrs mods name typeAnnotation optInitExpr)
      = string "var"
    <+> string name
    <> ppr typeAnnotation
    <+> maybe empty (\e -> equals <+> ppr e) optInitExpr

instance Pretty PatternInitializer where
  ppr (PatternInitializer pattern optExpression) = ppr pattern <+> string "=" <+> ppr optExpression

instance Pretty Pattern where
  ppr (WildcardPattern optTypeAnn) = string "_" <+> ppr optTypeAnn
  ppr (IdentifierPattern identifierPattern optTypeAnn) = string identifierPattern <> ppr optTypeAnn
-- pattern → value-binding-pattern­
  ppr (TuplePattern tuplePatterns optTypeAnn)
    = parens (commasep (map ppr tuplePatterns)) <+> ppr optTypeAnn
-- pattern → enum-case-pattern­
-- pattern → optional-pattern­
-- pattern → type-casting-pattern­
  ppr (ExpressionPattern expression) = ppr expression

instance Pretty CodeBlock where
  ppr (CodeBlock statements) = bracesLines (map ppr statements)

instance Pretty TypeAnnotation where
  ppr (TypeAnnotation attrs type_) = sepBySpace attrs <> string ":" <+> ppr type_
