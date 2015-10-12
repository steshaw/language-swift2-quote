{-# LANGUAGE OverloadedStrings  #-}

import Language.Swift.Quote.Syntax
import Language.Swift.Quote.Pretty

import Control.Arrow (right)
import qualified Language.Swift.Quote.Parser as P
import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Text.PrettyPrint.Mainland

main :: IO ()
main = defaultMain $ testGroup "Tests " [src2ast, src2ast2src]

litIntExp :: Integer -> Expression
litIntExp i = litExp (IntegerLiteral i)

src2ast :: TestTree
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

import_ :: Maybe ImportKind -> ImportPath -> Declaration
import_ = ImportDeclaration (Just DummyAttributes)

src2ast2src :: TestTree
src2ast2src = testGroup "Source -> AST -> Source"
  [ ppExpTest "1" "1"
  , ppExpTest "2 " "2"
  , ppExpTest " 3" "3"
  , ppExpTest " 4 " "4"
  , ppExpTest "\"Hello\"" "\"Hello\""
  , ppExpTest "\"foo\"" "\"foo\""
  , ppExpTest " \"x\"" "\"x\""
  , ppExpTest " \"y\" " "\"y\""
  , ppExpTest " true " "true"
  , ppExpTest "true" "true"
  , ppExpTest " \t false   " "false"
  , ppExpTest "self" "self"
  , ppExpTest "self . id" "self.id"
  , ppExpTest "self . init" "self.init"
  , ppExpTest "self [1,2,  3] " "self[1, 2, 3]"
  , ppFunctionCall "foo()" "foo()"
  , ppFunctionCall "foo( ) " "foo()"
  , ppFunctionCall "foo ( ) " "foo()"
  , ppFunctionCall "foo (false ) " "foo(false)"
  , ppFunctionCall "foo (a ) " "foo(a)"
  , ppFunctionCall "foo ( 1, 2 , isFred : true)" "foo(1, 2, isFred: true)"
  ]

-- parseFunctionCall "foo(1, 2, isFred: true)"

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

wrap :: String -> String
wrap s = "[[" ++ s ++ "]]"

expressionTest :: T.Text -> Expression -> TestTree
expressionTest input expression = testCase ("Expression: " ++ wrap (T.unpack input)) $
  P.parseExpression input @?= Right expression

declarationTest :: T.Text -> Declaration -> TestTree
declarationTest input declaration = testCase ("Declaration: " ++ wrap (T.unpack input)) $
  P.parseDeclaration input @?= Right declaration

pp :: Pretty pretty => Either d pretty -> Either d L.Text
pp = right (prettyLazyText 100 . ppr)

ppExpTest :: T.Text -> String -> TestTree
ppExpTest input s = testCase ("Expression " ++ wrap (T.unpack input) ++ " => " ++ wrap s) $
  sosrc @?= Right s
    where ast = P.parseExpression input
          osrc = pp ast
          sosrc = fmap L.unpack osrc

ppFunctionCall :: T.Text -> String -> TestTree
ppFunctionCall input s = testCase ("FunctionCall " ++ wrap (T.unpack input) ++ " => " ++ wrap s) $
  sosrc @?= Right s
    where ast = P.parseFunctionCall input
          osrc = pp ast
          sosrc = fmap L.unpack osrc
