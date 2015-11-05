{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Swift.Quote.Pretty where

import Language.Swift.Quote.Syntax

import Data.Text.Lazy (Text, append)
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

indentLevel :: Int
indentLevel = 2

ind :: Doc -> Doc
ind = indent indentLevel

sepBySpace :: Pretty p => [p] -> Doc
sepBySpace [] = empty
sepBySpace p = sep (map ppr p)

ppExps :: [Expression] -> Doc
ppExps expressions = commasep (map ppr expressions)

ppBracketExps :: [Expression] -> Doc
ppBracketExps = brackets . ppExps

ppExpressionElements :: [ExpressionElement] -> Doc
ppExpressionElements = parens . commasep . map ppr

ppBlock :: [Statement] -> Doc
ppBlock statements = bracesLines (map ppr statements)

ppStatementStack :: [Statement] -> Doc
ppStatementStack statements = stack (map ppr statements)

------------------------------------------------------------
-- Pretty instances
------------------------------------------------------------

instance Pretty Module where
  ppr (Module statements) = ppStatementStack statements

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
  ppr (Closure statements) = ppBlock statements

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
        pprTuple (e1, e2) = ppr e1 <> string ":" <+> ppr e2

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
  ppr (TypeIdentifierType ti) = ppr ti
  ppr (TypeOpt ty) = ppr ty <> string "?"
  ppr (ImplicitlyUnwrappedOptType ty) = ppr ty <> string "!"
  ppr (ArrayType ty) = brackets (ppr ty)
  ppr (DictionaryType t1 t2) = brackets (ppr t1 <> colon <+> ppr t2)
  ppr (FunctionType throws t1 t2)
    = ppr t1 <+> ppr throws <+> string "->" <+> ppr t2
  ppr (TypeMetaType t) = ppr t <> dot <> "Type"
  ppr (ProtocolMetaType t) = ppr t <> dot <> "Protocol"
  ppr (ProtocolCompositionType ids)
    = string "protocol"
    <> angles (commasep (map ppr ids))

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
  ppr (DeferStatement block) = string "defer" <+> ppr block
  ppr (ThrowStatement expression) = string "throw" <+> ppr expression
  ppr (DoStatement block clauses) = string "do" <+> ppr block <+> (spread . map ppr) clauses
  ppr (SwitchStatement e cases) = string "switch" <+> ppr e <+> bracesLines (map ppr cases)
  ppr (BreakStatement optLabelName) = string "break" <+> ppr optLabelName

instance Pretty Case where
  ppr (CaseLabel patternWheres statements)
    = string "case:"
      <+> commasep (map ppPatternWhere patternWheres)
      <+> colon
      <+> line
      <+> ind (ppStatementStack statements)
    where
      ppPatternWhere (pattern, Nothing) = ppr pattern
      ppPatternWhere (pattern, Just whereExp) = ppr pattern <+> string "where" <+> ppr whereExp
  ppr (CaseDefault statements)
    = string "default: "
      <+> line
      <+> ind (ppStatementStack statements)

instance Pretty CatchClause where
  ppr (CatchClause optPattern optWhere block)
    = string "catch" <+> ppr optPattern <+> ppr optWhere <+> ppr block

instance Pretty WhereClause where
  ppr (WhereClause e) = ppr e

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
    <> ppr optGenericParamClause
    <> sep (map (parens . commasep . map ppr) parameterClauses)
    <+> ppr optResult
    <+> ppr optBlock

  ppr (EnumDeclaration enum) = ppr enum

  ppr (StructDeclaration structType attrs optMod name optGPC optTIC decls)
    = sepBySpace attrs
    <+> ppr optMod
    <+> ppr structType
    <+> string name
    <> ppr optGPC
    <> ppr optTIC
    <> string " "
    <> bracesLines (map ppr decls)

  ppr (InitializerDeclaration atts mods kind optGPC paramClause throws block)
      = sepBySpace atts
    <+> sepBySpace mods
    <+> ppr kind
    <> ppr optGPC
    <> (parens . commasep . map ppr) paramClause
    <+> ppr throws
    <+> ppr block

  ppr (DeinitializerDeclaration atts block) = sepBySpace atts <+> string "deinit" <+> ppr block

  ppr (ExtensionDeclaration optDM ti optTIC body)
    = ppr optDM
    <+> string "extension"
    <+> ppr ti
    <> ppr optTIC
    <+> ppr body

  ppr (SubscriptDeclaration attrs mods params rAttrs result subscriptBlock)
      = sepBySpace attrs
    <+> sepBySpace mods
    <+> string "subscript"
    <> (parens . commasep . map ppr) params
    <+> "->"
    <+> sepBySpace rAttrs
    <+> ppr result
    <+> ppr subscriptBlock

  ppr (OperatorDeclaration opDecl) = ppr opDecl

  ppr (ProtocolDeclaration attrs optMod name optTIC members)
      = sepBySpace attrs
    <+> ppr optMod
    <+> string "protocol"
    <+> string name
    <> ppr optTIC
    <+> ppr members

instance Pretty ProtocolMembers where
  ppr (ProtocolMembers members) = bracesLines (map ppr members)

instance Pretty ProtocolMember where
  ppr (ProtocolAssociatedTypeDeclaration attrs optMod name optTIC optType)
      = sepBySpace attrs
    <+> ppr optMod
    <+> string "typealias"
    <+> ppr name
    <+> ppr optTIC
    <+> ppTypeAssign optType
      where
        ppTypeAssign Nothing = empty
        ppTypeAssign (Just type_) = string "=" <+> ppr type_

  ppr (ProtocolMethodDeclaration attrs mods name optGPC paramClauses optThrows optResult)
      = sepBySpace attrs
    <+> sepBySpace mods
    <+> string "func"
    <+> ppr name
    <> ppr optGPC
    <> sep (map (parens . commasep . map ppr) paramClauses)
    <+> ppr optResult

instance Pretty SubscriptBlock where
  ppr (SubscriptCodeBlock codeBlock) = ppr codeBlock
  ppr (SubscriptGetSetBlock getSetBlock) = ppr getSetBlock

instance Pretty GetSetBlock where
  ppr (GetSetBlock codeBlock) = ppr codeBlock
  ppr (GetSet optGet optSet) = ppr optGet <+> ppr optSet

instance Pretty GetterClause where
  ppr (GetterClause attrs block) = sepBySpace attrs <+> string "get" <+> ppr block

instance Pretty SetterClause where
  ppr (SetterClause attrs optIdent codeBlock)
    = sepBySpace attrs
    <+> string "set"
    <+> maybe empty (braces . string) optIdent
    <+> ppr codeBlock

instance Pretty TypeInheritanceClause where
  ppr (TypeInheritanceClause True typeIds) = string ":" <+> "class" <+> (commasep . map ppr) typeIds
  ppr (TypeInheritanceClause False typeIds) = string ":" <+> (commasep . map ppr) typeIds

instance Pretty ExtensionBody where
  ppr (ExtensionBody decls) = bracesLines (map ppr decls)

instance Pretty InitKind where
  ppr Init = string "init"
  ppr InitOption = string "init?"
  ppr InitForce = string "init!"

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
  ppr (Attribute name optClause) = string "@" <> ppr name <> ppClause optClause
    where
      ppClause Nothing = empty
      ppClause (Just s) = parens (string s)

instance Pretty DeclarationModifier where
  ppr (Modifier s) = ppr s
  ppr (AccessLevelModifier s) = ppr s

instance Pretty ImportPathIdentifier where
  ppr (ImportIdentifier s) = ppr s
  ppr (ImportOperator s) = ppr s

instance Pretty VariableDeclaration where
  ppr (VarDeclPattern attrs mods patternInitialisers) = string "var" <+> commasep (map ppr patternInitialisers)
  ppr (VarDeclObserved attrs mods name ta optInit observedBlock)
      = string "var"
    <+> string name
    <> ppr ta
    <+> maybe empty (\e -> equals <+> ppr e) optInit -- XXX
    <+> ppr observedBlock
  ppr (VarDeclReadOnly attrs mods name ta block)
      = string "var"
    <+> string name
    <> ppr ta
    <+> ppr block

instance Pretty ObservedBlock where
  ppr ObservedBlock = string ""

instance Pretty PatternInitializer where
  ppr (PatternInitializer pattern Nothing) = ppr pattern
  ppr (PatternInitializer pattern (Just e)) = ppr pattern <+> string "=" <+> ppr e

instance Pretty Pattern where
  ppr (WildcardPattern optTypeAnn) = string "_" <+> ppr optTypeAnn
  ppr (IdentifierPattern identifierPattern optTypeAnn) = string identifierPattern <> ppr optTypeAnn
-- pattern → value-binding-pattern­
  ppr (VarPattern pattern) = string "var" <+> ppr pattern
  ppr (LetPattern pattern) = string "let" <+> ppr pattern
  ppr (TuplePattern tuplePatterns optTypeAnn)
    = parens (commasep (map ppr tuplePatterns)) <+> ppr optTypeAnn
-- pattern → enum-case-pattern­
  ppr (OptionalPattern ident) = ppr ident <> string "?"
-- pattern → optional-pattern­
-- pattern → type-casting-pattern­
  ppr (ExpressionPattern expression) = ppr expression
  ppr (VarPattern pattern) = string "var" <+> ppr pattern
  ppr (LetPattern pattern) = string "let" <+> ppr pattern
  ppr (IsPattern ty) = string "is" <+> ppr ty
  ppr (AsPattern ty) = string "as" <+> ppr ty

instance Pretty CodeBlock where
  ppr (CodeBlock statements) = ppBlock statements

instance Pretty TypeAnnotation where
  ppr (TypeAnnotation attrs type_) = sepBySpace attrs <> string ":" <+> ppr type_

instance Pretty GenericParameterClause where
  ppr (GenericParameterClause params optReqClause) = angles (commasep (map ppr params))

instance Pretty GenericParameter where
  ppr (GenericParamName typeName) = ppr typeName
  ppr (GenericParamTypeId ti1 ti2) = ppr ti2 <> colon <+> ppr ti2

instance Pretty TypeIdentifier where
  ppr (TypeIdentifier typeArguments) = (cat . punctuate dot) (map p typeArguments)
    where
      p :: (TypeName, [Type]) -> Doc
      p (n, []) = ppr n
      p (n, ts) = ppr n <> (angles . commasep) (map ppr ts)

instance Pretty OperatorDecl where
  ppr (PrefixOperatorDecl op) = string "prefix" <+> "operator" <+> string op <+> string "{}"
  ppr (PostfixOperatorDecl op) = string "postfix" <+> "operator" <+> string op <+> string "{}"
  ppr (InfixOperatorDecl op optP optA)
    = string "infix" <+> "operator" <+> string op
      <+> braces (ppPrec optP <+> ppr optA)
      where
        ppPrec Nothing = empty
        ppPrec (Just prec) = string "precedence" <+> ppr prec

instance Pretty Associativity where
  ppr AssocLeft  = string "associativity" <+> string "left"
  ppr AssocRight = string "associativity" <+> string "right"
  ppr AssocNone  = string "associativity" <+> string "none"

instance Pretty InOut where
  ppr InOut = string "inout"

instance Pretty ConditionClause where
  ppr (ConditionClause optE conditions) = ppr optE <> ppr conditions

instance Pretty ConditionList where
  ppr (ConditionList conditions) = commasep (map ppr conditions)

instance Pretty Condition where
  ppr (CaseCondition p e optWC)
    = string "case"
    <+> ppr p
    <+> string "="
    <+> ppr e
    <+> ppr optWC
  ppr (AvailabilityCondition args) = string "#availability" <> parens (commasep (map ppr args))

instance Pretty AvailabilityArgument where
  ppr (PlatformAvailabilityArgument name version) = string "<todo platform name version>"
  ppr PlatformWildcard = string "_"

instance Pretty Initializer where
  ppr (Initializer e) = ppr e
