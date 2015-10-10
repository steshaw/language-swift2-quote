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

litIntExp i = litExp (IntegerLiteral i)

src2ast = testGroup "Source -> AST"
  [ expressionTest "1" $ litExpMod (IntegerLiteral 1)
  , expressionTest " 2" $ litExpMod (IntegerLiteral 2)
  , expressionTest "3 " $ litExpMod (IntegerLiteral 3)
  , expressionTest " 4 " $ litExpMod (IntegerLiteral 4)
  , expressionTest "\"Hello\"" $ litExpMod (StringLiteral "Hello")
  , expressionTest " \"Hello\"" $ litExpMod (StringLiteral "Hello")
  , expressionTest "\"Hello\" " $ litExpMod (StringLiteral "Hello")
  , expressionTest " \"Hello\" " $ litExpMod (StringLiteral "Hello")
  , expressionTest "true" $ litExpMod (BooleanLiteral True)
  , expressionTest "false" $ litExpMod (BooleanLiteral False)
  , expressionTest " true" $ litExpMod (BooleanLiteral True)
  , expressionTest "true " $ litExpMod (BooleanLiteral True)
  , expressionTest " true " $ litExpMod (BooleanLiteral True)
  , expressionTest " false" $ litExpMod (BooleanLiteral False)
  , expressionTest "false " $ litExpMod (BooleanLiteral False)
  , expressionTest " false " $ litExpMod (BooleanLiteral False)
  , expressionTest "self" $ self Self1
  , expressionTest "self.a" $ self (Self2 "a")
  , expressionTest "self. a" $ self (Self2 "a")
  , expressionTest "self . a" $ self (Self2 "a")
  , expressionTest " self . a" $ self (Self2 "a")
  , expressionTest " self . a " $ self (Self2 "a")
  , expressionTest "self[1]" $ self (Self3 [litIntExp 1])
  , expressionTest "self[1,2]" $ self (Self3 [litIntExp 1, litIntExp 2])
  , expressionTest "self[1, 2]" $ self (Self3 [litIntExp 1, litIntExp 2])
  , expressionTest "self [1, 2]" $ self (Self3 [litIntExp 1, litIntExp 2])
  , expressionTest "self [ 1, 2 ]" $ self (Self3 [litIntExp 1, litIntExp 2])
  , expressionTest "self.init" $ self Self4
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
  , ppTest "self" "self"
  , ppTest "self . id" "self.id"
  , ppTest "self . init" "self.init"
  , ppTest "self [1,2,  3] " "self[1, 2, 3]"
  ]

litExpMod :: Literal -> Module
litExpMod lit = Module $ litExp lit

litExp :: Literal -> Expression
litExp lit =
  Expression1 Nothing
    (PeRegular Nothing
      (PrimaryExpression1
        (RegularLiteral lit))) (Just [])

self :: SelfExpression -> Module
self se = Module
  (Expression1 Nothing
    (PeRegular Nothing
      (PrimaryExpression2
        se)) (Just []))

expressionTest :: T.Text -> Module -> TestTree
expressionTest input expressionModule = testCase ("Expression " ++ T.unpack input) $
  parse input @?= Right expressionModule

wrap :: String -> String
wrap s = "[[" ++ s ++ "]]"
indent = "  "

-- ppTest :: T.Text -> String -> TestTree
ppTest input s = testCase ("Literal " ++ T.unpack input) $
  sosrc @?= Right s
    where ast = parse input
          osrc = fmap prettyPrint ast
          sosrc = fmap L.unpack osrc
