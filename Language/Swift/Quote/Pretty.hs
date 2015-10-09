module Language.Swift.Quote.Pretty where

import Language.Swift.Quote.Syntax

import Data.Text.Lazy (Text)
import Text.PrettyPrint.Mainland

prettyPrint :: Module -> Text
prettyPrint (Module expression) = prettyLazyText 100 $ ppr expression

instance Pretty Expression where
  ppr (IntegerLiteral n) = integer n
  ppr (StringLiteral s) = dquotes (string s)
  ppr (BooleanLiteral b) = if b then text "true" else text "false"
  ppr (NilLiteral nil) = text "nil"
  ppr (PrefixExpression p) = text "prefix-expression not implemented"
  ppr DummyExpression = text "<dummy>"
