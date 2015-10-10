{-# LANGUAGE OverloadedStrings  #-}

import Test.Tasty
import Test.Tasty.HUnit

import Language.Swift.Quote.Parser
import Language.Swift.Quote.Syntax
import Language.Swift.Quote.Pretty
import qualified Data.Text.Lazy as L
import qualified Data.Text as T

main :: IO ()
main = defaultMain $ testGroup "Tests " [src2ast, src2ast2src]

src2ast = testGroup "Source -> AST"
  [ litTest "1" (IntegerLiteral 1)
  , litTest " 2" (IntegerLiteral 2)
  , litTest "3 " (IntegerLiteral 3)
  , litTest " 4 " (IntegerLiteral 4)
  , litTest "\"Hello\"" (StringLiteral "Hello")
  , litTest " \"Hello\"" (StringLiteral "Hello")
  , litTest "\"Hello\" " (StringLiteral "Hello")
  , litTest " \"Hello\" " (StringLiteral "Hello")
  , litTest "true" (BooleanLiteral True)
  , litTest "false" (BooleanLiteral False)
  , litTest " true" (BooleanLiteral True)
  , litTest "true " (BooleanLiteral True)
  , litTest " true " (BooleanLiteral True)
  , litTest " false" (BooleanLiteral False)
  , litTest "false " (BooleanLiteral False)
  , litTest " false " (BooleanLiteral False)
  ]

src2ast2src = testGroup "Source -> AST -> Source"
  [ ppTest "1" "1"
  , ppTest "2 " "2"
  , ppTest " 3" "3"
  , ppTest " 4 " "4"
  , ppTest "\"Hello\"" "\"Hello\""
  , ppTest "\"foo\"" "\"foo\""
  , ppTest " \"x\"" "\"x\""
  , ppTest " \"y\" " "\"y\""
  , ppTest " true " "true"
  , ppTest "true" "true"
  , ppTest " \t false   " "false"
  ]

litMod :: Literal -> Module
litMod lit = Module
  (Expression1 Nothing
    (PeRegular Nothing
      (PrimaryExpression1
        (RegularLiteral lit))) (Just []))

litTest :: T.Text -> Literal -> TestTree
litTest input i = testCase ("Literal " ++ T.unpack input) $
  parse input @?= Right (litMod i)

wrap :: String -> String
wrap s = "[[" ++ s ++ "]]"
indent = "  "

-- ppTest :: T.Text -> String -> TestTree
ppTest input s = testCase ("Literal " ++ T.unpack input) $
  sosrc @?= Right s
    where ast = parse input
          osrc = fmap prettyPrint ast
          sosrc = fmap L.unpack osrc
