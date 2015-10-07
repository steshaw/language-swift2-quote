module Language.Swift.Quote.Parser where

import Language.Swift.Quote.Syntax

import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec.Text
import qualified Text.ParserCombinators.Parsec.Token as Token

parse :: Text -> Either String Module
parse input = Left "unimplemented"

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
  , "while."
  ]

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

keywordsInPatterns = pure "_"

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

reservedOperators =
  [ "("
  , ")"
  , "{"
  , "}"
  , "["
  , "]"
  , "."
  , ","
  , ":"
  , ";"
  , "="
  , "@"
  , "#"
  , "&" -- as a prefix operator
  , "->"
  , "`"
  , "?"
  , "!"
  ]

-- Ignore Whitespace
-- C-style comments with nesting.
languageDef = emptyDef
  { Token.commentStart = "/*"
  , Token.commentEnd = "*/"
  , Token.commentLine = "//"
  , Token.identStart = letter
  , Token.identLetter = alphaNum
  , Token.reservedNames = reservedWordsDeclarations
                          ++ reservedWordsStatements
                          ++ reservedWordsExpressionsTypes
                          ++ keywordsInPatterns
                          ++ keywordsinContexts
  , Token.reservedOpNames = [ "+", "-", "*", "/", "<", ">", "<=", ">="]
  }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
integer = Token.integer lexer
ws = Token.whiteSpace lexer
