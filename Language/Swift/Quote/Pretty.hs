{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Swift.Quote.Pretty where

import Language.Swift.Quote.Syntax

import Data.Text.Lazy (Text)
import Text.PrettyPrint.Mainland

prettyPrint :: Module -> Text
prettyPrint = error "<module>" -- TODO

instance Pretty Expression where
  ppr (Expression optTryOperator prefixExpression optBinaryExpressions) =
    ppr optTryOperator <> ppr prefixExpression -- <> ppr optBinaryExpressions

instance Pretty PrefixExpression where
  ppr (PrefixOperator optPrefixOperator primaryExpression)
    = ppr optPrefixOperator <> ppr primaryExpression
  ppr (InOutExpression identifier) = string "&" <> string identifier

instance Pretty PostfixExpression where
  ppr (PostfixPrimary primaryExpression) = ppr primaryExpression
  ppr (PostfixOperator prefixExpression postfixOperator) = ppr prefixExpression <> ppr postfixOperator
  ppr (FunctionCallE functionCall) = ppr functionCall
  ppr (PostfixExpression4Initalizer postfixExpression) = ppr postfixExpression <> string ".init"
  ppr (PostfixSelf postfixExpression) = ppr postfixExpression <> string ".self"
  ppr (PostfixDynamicType postfixExpression) = ppr postfixExpression <> string ".dynamicType"
  ppr (PostfixForcedValue postfixExpression) = ppr postfixExpression <> string "!"
  ppr (PostfixOptionChaining postfixExpression) = ppr postfixExpression <> string "?"
  ppr (Subscript postfixExpression expressions) = ppr postfixExpression <> brackets (commasep (map ppr expressions))

instance Pretty FunctionCall where
  ppr (FunctionCall postfixExpression expressionElements optClosure) =
    ppr postfixExpression
      <> parens (commasep (map ppr expressionElements))
      <> ppr optClosure

instance Pretty ExpressionElement where
  ppr (ExpressionElement Nothing expression) = ppr expression
  ppr (ExpressionElement (Just ident) expression) = ppr ident <> colon <> space <> ppr expression

instance Pretty Closure where
  ppr (Closure []) = empty
  ppr (Closure statements) = braces (ppr statements)

instance Pretty PrimaryExpression where
  ppr (PrimaryExpression1 (IdG identifier genericArgumentList)) =
    ppr identifier <> if null genericArgumentList then string "" else angles (ppr genericArgumentList)
  ppr (PrimaryExpression2 literalExpression) = ppr literalExpression
  ppr (PrimaryExpression3 selfExpression) = ppr selfExpression
  ppr (PrimaryExpression4 superclassExpression) = ppr superclassExpression
  ppr (PrimaryExpression5 closure) = ppr closure
  ppr (PrimaryExpression6 expressionElements) = ppr expressionElements
  ppr PrimaryExpression7 = string "<implicit-member-expression>" -- TODO implicit-member-expression
  ppr PrimaryExpression8 = string "_"

instance Pretty BinaryExpression where
  ppr (BinaryExpression1 operator prefixExpression) = ppr operator <> ppr prefixExpression
  ppr (BinaryExpression2 tryOperator prefixExpression) = ppr tryOperator <> ppr prefixExpression
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
  ppr (Self3 expressions) = string "self" <> brackets (commasep (map ppr expressions))
  ppr Self4 = string "self" <> string "." <> string "init"

instance Pretty SuperclassExpression where
  ppr (SuperclassExpression) = string "<super>" -- TODO

instance Pretty Type where
  ppr (Type ty) = ppr ty

instance Pretty Statement where
  ppr DummyStatement = string "<dummy-statement>"
