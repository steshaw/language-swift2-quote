module Language.Swift.Quote.Pretty where

import Language.Swift.Quote.Syntax

import Data.Text.Lazy (Text)
import Text.PrettyPrint.Mainland

prettyPrint :: Module -> Text
prettyPrint (Module
  (Expression1 Nothing (PeRegular Nothing primaryExpression) (Just [])))
    = prettyLazyText 100 $ ppr primaryExpression

instance Pretty Expression where
  ppr (Expression1 optTryOperator prefixExpression optBinaryExpressions) = ppr prefixExpression

instance Pretty PrefixExpression where
  ppr (PeRegular optPrefixOperator primaryExpression) = ppr primaryExpression
  ppr (PeInOutExpression identifier) = string "&" <> string identifier

instance Pretty PrimaryExpression where
  ppr (PrimaryExpression1 literalExpression) =  ppr literalExpression
  ppr (PrimaryExpression2 selfExpression) =  ppr selfExpression

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
  ppr (Self3 expressions) = string "self" <> squotes (commasep (map ppr expressions))
  ppr Self4 = string "self" <> string "." <> string "init"
