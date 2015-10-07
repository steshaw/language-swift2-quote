module Language.Swift.Quote.Syntax where

import Data.Text (Text)

data Module = Module Expression
  deriving (Show, Eq)

data Expression
  = IntegerLiteral Integer
  | StringLiteral String
  | BooleanLiteral Bool
  | NilLiteral String
  deriving (Show, Eq)
