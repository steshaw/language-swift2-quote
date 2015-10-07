module Language.Swift.Quote.Parser where

import Language.Swift.Quote.Syntax

import Control.Applicative
import Control.Monad.Identity

import Data.Text
import qualified Text.ParserCombinators.Parsec as P
import Text.Parsec.Text (Parser)
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T

parse :: Text -> Either String Module
parse input = case P.parse moduleP "<stdin>" input of
  Left err -> Left $ show err
  Right mod -> Right mod

moduleP :: Parser Module
moduleP = Module <$> expression <* ws

------------------------------------------------------------
-- Lexical Structure
------------------------------------------------------------

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
swiftLangDef :: GenLanguageDef Text st Identity
swiftLangDef = T.LanguageDef
  { T.commentStart = "/*"
  , T.commentEnd = "*/"
  , T.commentLine = "//"
  , T.nestedComments = True
  , T.identStart = P.letter
  , T.identLetter = P.alphaNum
  , T.opStart = P.oneOf "+-*/<>="
  , T.opLetter = P.oneOf "+-*/<>="
  , T.reservedNames = reservedWordsDeclarations
                          ++ reservedWordsStatements
                          ++ reservedWordsExpressionsTypes
                          ++ keywordsInPatterns
                          ++ keywordsinContexts
  , T.reservedOpNames = [ "+", "-", "*", "/", "<", ">", "<=", ">="]
  , T.caseSensitive = True
  }

lexer = T.makeTokenParser swiftLangDef

identifier = T.identifier lexer
ws = T.whiteSpace lexer

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

integerLiteral :: Parser Expression
integerLiteral = IntegerLiteral <$> T.integer lexer

stringLiteral :: Parser Expression
stringLiteral = StringLiteral <$> T.stringLiteral lexer

booleanLiteral :: Parser Expression
booleanLiteral = BooleanLiteral <$>
     (P.string "true" *> pure True
  <|> P.string "false" *> pure False)

nilLiteral :: Parser Expression
nilLiteral = NilLiteral <$> P.string "nil"

literal :: Parser Expression
literal = ws *>
  (integerLiteral <|> stringLiteral <|> booleanLiteral <|> nilLiteral)

expression :: Parser Expression
expression = literal
