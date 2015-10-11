{-# LANGUAGE OverloadedStrings  #-}

import Control.Arrow (left)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Language.Swift.Quote.Parser as P
import Language.Swift.Quote.Syntax
import Language.Swift.Quote.Pretty
import qualified Data.Text.Lazy as L
import qualified Data.Text as T

main :: IO ()
main = defaultMain $ testGroup "Tests " [src2ast, src2ast2src]

litIntExp i = litExp (IntegerLiteral i)

src2ast = testGroup "Source -> AST"
  [ expressionTest "1" $ litIntExp 1
  , expressionTest " 2" $ litIntExp 2
  , expressionTest "3 " $ litIntExp 3
  , expressionTest " 10 " $ litIntExp 10
  , expressionTest "\"Hello\"" $ litExp (StringLiteral "Hello")
  , expressionTest " \"Hello\"" $ litExp (StringLiteral "Hello")
  , expressionTest "\"Hello\" " $ litExp (StringLiteral "Hello")
  , expressionTest " \"Hello\" " $ litExp (StringLiteral "Hello")
  , expressionTest "true" $ litExp (BooleanLiteral True)
  , expressionTest "false" $ litExp (BooleanLiteral False)
  , expressionTest " true" $ litExp (BooleanLiteral True)
  , expressionTest "true " $ litExp (BooleanLiteral True)
  , expressionTest " true " $ litExp (BooleanLiteral True)
  , expressionTest " false" $ litExp (BooleanLiteral False)
  , expressionTest "false " $ litExp (BooleanLiteral False)
  , expressionTest " false " $ litExp (BooleanLiteral False)
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
  , expressionTest "foo" $ primary1 "foo"
  , expressionTest "a" $ primary1 "a"
  , expressionTest "a1" $ primary1 "a1"
  , expressionTest "xs" $ primary1 "xs"
  , expressionTest "1 is Int" $ typeCastExp (IntegerLiteral 1) "is" (Type "Int")
  , expressionTest "200 as Double" $ typeCastExp (IntegerLiteral 200) "as" (Type "Double")
  , expressionTest "\"s\" as? String" $ typeCastExp (StringLiteral "s") "as?" (Type "String")
  , expressionTest "\"s\" as! String" $ typeCastExp (StringLiteral "s") "as!" (Type "String")
  , declarationTest "import foo" $ import_ Nothing (map ImportIdentifier ["foo"])
  , declarationTest "import foo.math.BitVector" $ import_ Nothing (map ImportIdentifier["foo", "math", "BitVector"])
  , declarationTest "import typealias foo.a.b" $ import_ (pure "typealias") (map ImportIdentifier ["foo", "a", "b"])
  ]

import_ = ImportDeclaration (Just DummyAttributes)

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

primary1 :: String -> Expression
primary1 identifier =
  Expression1 Nothing
    (PrefixExpression1 Nothing
      (PostfixExpression1
        (PrimaryExpression1 identifier Nothing))) (Just [])

typeCastExp :: Literal -> String -> Type -> Expression
typeCastExp lit typeCastKind type_ =
  Expression1 Nothing
    (PrefixExpression1 Nothing
      (PostfixExpression1
        (PrimaryExpression2
          (RegularLiteral lit)))) (Just [BinaryExpression4 typeCastKind type_])

litExp :: Literal -> Expression
litExp lit =
  Expression1 Nothing
    (PrefixExpression1 Nothing
      (PostfixExpression1
        (PrimaryExpression2
          (RegularLiteral lit)))) (Just [])

self :: SelfExpression -> Expression
self se =
  Expression1 Nothing
    (PrefixExpression1 Nothing
      (PostfixExpression1
        (PrimaryExpression3
          se))) (Just [])

expressionTest :: T.Text -> Expression -> TestTree
expressionTest input expression = testCase ("Expression: [[" ++ T.unpack input ++ "]]") $
  P.parseExpression input @?= Right expression

declarationTest :: T.Text -> Declaration -> TestTree
declarationTest input declaration = testCase ("Declaration: [[" ++ T.unpack input ++ "]]") $
  P.parseDeclaration input @?= Right declaration

wrap :: String -> String
wrap s = "[[" ++ s ++ "]]"
indent = "  "

-- ppTest :: T.Text -> String -> TestTree
ppTest input s = testCase ("Literal " ++ T.unpack input) $
  sosrc @?= Right s
    where ast = P.parseExpression input
          osrc = fmap ppExpression ast
          sosrc = fmap L.unpack osrc
