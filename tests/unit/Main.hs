{-# LANGUAGE OverloadedStrings  #-}

import Language.Swift.Quote.Syntax
import Language.Swift.Quote.Pretty

import Control.Arrow (right)
import qualified Language.Swift.Quote.Parser as P
import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import System.IO
import Test.Tasty
import Test.Tasty.HUnit
import Text.PrettyPrint.Mainland

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  defaultMain $ testGroup "Tests " [src2ast, src2ast2src]

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
  , expressionTest "&a" $ Expression Nothing (InOutExpression "a") []
  , expressionTest "& b" $ Expression Nothing (InOutExpression "b") []
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
  , expressionTest "a.123" $ Expression Nothing (PrefixOperator Nothing (ExplicitMemberExpressionDigits (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = Nothing}))) "123")) []
  , expressionTest "a.b" $ Expression Nothing (PrefixOperator Nothing (ExplicitMemberExpressionIdentifier (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = Nothing}))) (IdG {idgIdentifier = "b", idgGenericArgs = Nothing}))) []
  , expressionTest "foo" $ primary1 "foo"
  , expressionTest "a" $ primary1 "a"
  , expressionTest "a1" $ primary1 "a1"
  , expressionTest "xs" $ primary1 "xs"
  , expressionTest "1 is Int" $ typeCastExp (IntegerLiteral 1) "is" (Type "Int")
  , expressionTest "200 as Double" $ typeCastExp (IntegerLiteral 200) "as" (Type "Double")
  , expressionTest "\"s\" as? String" $ typeCastExp (StringLiteral "s") "as?" (Type "String")
  , expressionTest "\"s\" as! String" $ typeCastExp (StringLiteral "s") "as!" (Type "String")
  , expressionTest "a++" $ Expression Nothing (PrefixOperator Nothing
      (PostfixOperator (PostfixPrimary (PrimaryExpression1
        (IdG {idgIdentifier = "a", idgGenericArgs = Nothing}))) "++")) []
  , expressionTest "foo()" $ fooEmptyFunCall
  , expressionTest "foo(1)" $ Expression Nothing (PrefixOperator Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = Nothing}))) [ExpressionElement Nothing (Expression Nothing (PrefixOperator Nothing (PostfixPrimary (PrimaryExpression2 (RegularLiteral (IntegerLiteral 1))))) [])] Nothing))) []
  , expressionTest "foo(1, 2)" $ Expression Nothing (PrefixOperator Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = Nothing}))) [ExpressionElement Nothing (Expression Nothing (PrefixOperator Nothing (PostfixPrimary (PrimaryExpression2 (RegularLiteral (IntegerLiteral 1))))) []),ExpressionElement Nothing (Expression Nothing (PrefixOperator Nothing (PostfixPrimary (PrimaryExpression2 (RegularLiteral (IntegerLiteral 2))))) [])] Nothing))) []
  , expressionTest "foo(1, 2, isBlue: false)" $ Expression Nothing (PrefixOperator Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = Nothing}))) [ExpressionElement Nothing (Expression Nothing (PrefixOperator Nothing (PostfixPrimary (PrimaryExpression2 (RegularLiteral (IntegerLiteral 1))))) []),ExpressionElement Nothing (Expression Nothing (PrefixOperator Nothing (PostfixPrimary (PrimaryExpression2 (RegularLiteral (IntegerLiteral 2))))) []),ExpressionElement (Just "isBlue") (Expression Nothing (PrefixOperator Nothing (PostfixPrimary (PrimaryExpression2 (RegularLiteral (BooleanLiteral False))))) [])] Nothing))) []
  , expressionTest "1.init" $ Expression Nothing (PrefixOperator Nothing (PostfixExpression4Initalizer (PostfixPrimary (PrimaryExpression2 (RegularLiteral (IntegerLiteral 1)))))) []
  , expressionTest "a.init" $ Expression Nothing (PrefixOperator Nothing (PostfixExpression4Initalizer (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = Nothing}))))) []
  , expressionTest "foo.init" $ Expression Nothing (PrefixOperator Nothing (PostfixExpression4Initalizer (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = Nothing}))))) []
  , expressionTest "a.self" $ Expression Nothing (PrefixOperator Nothing (PostfixSelf (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = Nothing}))))) []
  , expressionTest "a.dynamicType" $ Expression Nothing (PrefixOperator Nothing (PostfixDynamicType (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = Nothing}))))) []
  , expressionTest "a!" $ Expression Nothing (PrefixOperator Nothing (PostfixForcedValue (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = Nothing}))))) []
  , expressionTest "a?" $ Expression Nothing (PrefixOperator Nothing (PostfixOptionChaining (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = Nothing}))))) []
  , expressionTest "a[1]" $ Expression Nothing (PrefixOperator Nothing (Subscript (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = Nothing}))) [Expression Nothing (PrefixOperator Nothing (PostfixPrimary (PrimaryExpression2 (RegularLiteral (IntegerLiteral 1))))) []])) []

  , moduleTest "import foo" $ singleImport Nothing ["foo"]
  , moduleTest "import foo.math.BitVector" $ singleImport Nothing ["foo", "math", "BitVector"]
  , moduleTest "import typealias foo.a.b" $ singleImport (Just "typealias") ["foo", "a", "b"]

  , moduleTest "print(\"Hello world\\n\")" $ Module [ExpressionStatement (Expression Nothing (PrefixOperator Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "print", idgGenericArgs = Nothing}))) [ExpressionElement Nothing (Expression Nothing (PrefixOperator Nothing (PostfixPrimary (PrimaryExpression2 (RegularLiteral (StringLiteral "Hello world\n"))))) [])] Nothing))) [])]
  ]

emptyModule = Module []

singleImport optImportKind imports = Module [DeclarationStatement (import_ optImportKind (map ImportIdentifier imports))]

fooEmptyFunCall = (Expression Nothing (PrefixOperator Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = Nothing}))) [] Nothing))) [])

initTest1 :: PostfixExpression
initTest1 = PostfixExpression4Initalizer (PostfixPrimary (PrimaryExpression2 (RegularLiteral (IntegerLiteral 1))))

initTest2 :: PostfixExpression
initTest2 = PostfixExpression4Initalizer (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = Nothing})))

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
  , ppExpTest "foo()" "foo()"
  , ppExpTest "foo( ) " "foo()"
  , ppExpTest "foo ( ) " "foo()"
  , ppExpTest "foo (false ) " "foo(false)"
  , ppExpTest "foo (a ) " "foo(a)"
  , ppExpTest "foo ( 1, 2 , isFred : true)" "foo(1, 2, isFred: true)"
  ]

primary1 :: String -> Expression
primary1 identifier =
  Expression Nothing
    (PrefixOperator Nothing
      (PostfixPrimary
        (PrimaryExpression1 (IdG identifier Nothing)))) []

typeCastExp :: Literal -> String -> Type -> Expression
typeCastExp lit typeCastKind type_ =
  Expression Nothing
    (PrefixOperator Nothing
      (PostfixPrimary
        (PrimaryExpression2
          (RegularLiteral lit)))) [BinaryExpression4 typeCastKind type_]

litExp :: Literal -> Expression
litExp lit =
  Expression Nothing
    (PrefixOperator Nothing
      (PostfixPrimary
        (PrimaryExpression2
          (RegularLiteral lit)))) []

self :: SelfExpression -> Expression
self se =
  Expression Nothing
    (PrefixOperator Nothing
      (PostfixPrimary
        (PrimaryExpression3
          se))) []

wrap :: String -> String
wrap s = "[[" ++ s ++ "]]"

moduleTest :: T.Text -> Module -> TestTree
moduleTest input module_ = testCase ("module: " ++ wrap (T.unpack input)) $
  P.parse input @?= Right module_

expressionTest :: T.Text -> Expression -> TestTree
expressionTest input expression = testCase ("expression: " ++ wrap (T.unpack input)) $
  P.parseExpression input @?= Right expression

pp :: Pretty pretty => Either d pretty -> Either d L.Text
pp = right (prettyLazyText 100 . ppr)

ppExpTest :: T.Text -> String -> TestTree
ppExpTest input s = testCase ("expression " ++ wrap (T.unpack input) ++ " => " ++ wrap s) $
  sosrc @?= Right s
    where ast = P.parseExpression input
          osrc = pp ast
          sosrc = fmap L.unpack osrc
