module Language.Swift.Quote.Syntax where

import Data.Text (Text)

data Module = Module ([Expression])

data Expression
  = IntegerLiteral Integer
  | StringLiteral String
  | BooleanLiteral String
  | NilLiteral String
