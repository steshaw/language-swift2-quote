{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Swift.Quote.Pretty where

import Language.Swift.Quote.Syntax

import Data.Text.Lazy (Text, append)
import Text.PrettyPrint.Mainland

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
  ppr (ExplicitMemberExpressionDigits postfixExpression digits) = ppr postfixExpression <> string digits
  ppr (ExplicitMemberExpressionIdentifier postfixExpression idG) = ppr postfixExpression <> ppr idG
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
  ppr (ExpressionElement (Just ident) expression) = ppr ident <> colon <> space <> ppr expression

instance Pretty Closure where
  ppr (Closure []) = empty
  ppr (Closure statements) = braces (ppr statements)

instance Pretty PrimaryExpression where
  ppr (PrimaryExpression1 idG) = ppr idG
  ppr (PrimaryLiteral literalExpression) = ppr literalExpression
  ppr (PrimarySelf selfExpression) = ppr selfExpression
  ppr (PrimarySuper superclassExpression) = ppr superclassExpression
  ppr (PrimaryClosure closure) = ppr closure
  ppr (PrimaryParenthesized [expressionElement]) = ppr expressionElement -- FIXME hack because more expressions parse as PrimaryParenthesized than should do.
  ppr (PrimaryParenthesized expressionElements) = (parens . cat .  map ppr) expressionElements
  ppr PrimaryImplicitMember = string "<implicit-member-expression>" -- TODO implicit-member-expression
  ppr PrimaryWildcard = string "_"

instance Pretty IdG where
  ppr (IdG identifier []) = ppr identifier
  ppr (IdG identifier genericArgs) = ppr identifier <> angles (ppr genericArgs)

instance Pretty BinaryExpression where
  ppr (BinaryExpression1 operator prefixExpression) = ppr operator <+> ppr prefixExpression
  ppr (BinaryAssignmentExpression tryOperator prefixExpression) = string "=" <+> ppr tryOperator <+> ppr prefixExpression
  ppr (BinaryExpression3 (optS1, expression) optS2 prefixExpression) =
    ppr optS1 <> ppr expression <> ppr optS2 <> ppr prefixExpression
  ppr (BinaryExpression4 s typ) = ppr s <> ppr typ

instance Pretty LiteralExpression where
  ppr (RegularLiteral lit) =  ppr lit
  ppr (SpecialLiteral special) =  ppr special

instance Pretty Literal where
  ppr (IntegerLiteral n) = integer n
  ppr (FloatingPointLiteral d) = double d
  ppr (StringLiteral s) = dquotes (string s)
  ppr (BooleanLiteral b) = if b then text "true" else text "false"
  ppr NilLiteral = text "nil"

instance Pretty SelfExpression where
  ppr Self1 = string "self"
  ppr (Self2 identifier) = string "self" <> string "." <> string identifier
  ppr (Self3 expressions) = string "self" <> ppBracketExps expressions
  ppr Self4 = string "self" <> string "." <> string "init"

instance Pretty SuperclassExpression where
  ppr (SuperclassExpression) = string "<super>" -- TODO

instance Pretty Type where
  ppr (Type ty) = ppr ty

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

  ppr DummyDeclaration = string "<dummy-decl>"

instance Pretty FunctionName where
  ppr (FunctionNameIdent s) = string s
  ppr (FunctionNameOp s) = string s

instance Pretty FunctionResult where
  ppr (FunctionResult attrs type_) = string "->" <+> sepBySpace attrs <+> ppr type_

instance Pretty Parameter where
  ppr (ParameterLet optExternName localName typeAnnotation optExpression)
    = string localName
    <> string ":"
    <+> ppr typeAnnotation
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
  ppr (SimpleVariableDeclaration patternInitialisers) = string "var" <+> commasep (map ppr patternInitialisers)

instance Pretty PatternInitializer where
  ppr (PatternInitializer (ExpressionPattern expression) optExpression) = ppr expression <+> ppr optExpression -- FIXME We currently get an Assignment ConstantDeclaration(AssignmentExpression) instead of ConstantDeclaration(IdentifierExpression, Expression)
  ppr (PatternInitializer pattern optExpression) = ppr pattern <+> string "=" <+> string "[[" <+> ppr optExpression <+> string "]]"

instance Pretty Pattern where
  ppr (ExpressionPattern expression) = ppr expression

instance Pretty CodeBlock where
  ppr (CodeBlock statements) = lbrace <> line
    <> ind (stack (map ppr statements))
    <> line <> rbrace

instance Pretty TypeAnnotation where
  ppr (TypeAnnotation attrs type_) = sepBySpace attrs <+> ppr type_
