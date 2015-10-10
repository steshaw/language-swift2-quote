module Language.Swift.Quote.Pretty where

import Language.Swift.Quote.Syntax

import Data.Text.Lazy (Text)
import Text.PrettyPrint.Mainland

prettyPrint :: Module -> Text
prettyPrint (Module
  (Expression1 Nothing (PrefixExpression1 Nothing primaryExpression) (Just [])))
    = prettyLazyText 100 $ ppr primaryExpression

instance Pretty Expression where
  ppr (Expression1 optTryOperator prefixExpression optBinaryExpressions) = ppr prefixExpression

instance Pretty PrefixExpression where
  ppr (PrefixExpression1 optPrefixOperator primaryExpression) = ppr primaryExpression
  ppr (PrefixExpression2 identifier) = string "&" <> string identifier

instance Pretty PostfixExpression where
  ppr (PostfixExpression1 primaryExpression) = ppr primaryExpression
  ppr (PostfixExpression2 optPrefixOperator primaryExpression) = string "<TODO>" -- TODO
  ppr (PostfixExpression3 functionCall) = ppr functionCall

instance Pretty FunctionCall where
  ppr (FunctionCall postfixExpression optOptExpressionElements optClosure) =
    ppr postfixExpression <> ppr optOptExpressionElements <> ppr optClosure
    -- TODO correct this with brackets and braces as required.

instance Pretty ExpressionElement where
  ppr (ExpressionElement optString expression) = ppr optString <> ppr expression

instance Pretty Closure where
  ppr (Closure) = string "<closure>"

instance Pretty PrimaryExpression where
  ppr (PrimaryExpression1 identifier genericArgumentList) = ppr identifier <> angles (ppr genericArgumentList)
  ppr (PrimaryExpression2 literalExpression) =  ppr literalExpression
  ppr (PrimaryExpression3 selfExpression) =  ppr selfExpression

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

instance Pretty Type where
  ppr (Type string) = ppr string
