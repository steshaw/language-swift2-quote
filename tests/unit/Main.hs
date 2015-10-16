{-# LANGUAGE OverloadedStrings  #-}

import Language.Swift.Quote.Syntax
import Language.Swift.Quote.Pretty

import Control.Arrow (left, right)
import Language.Swift.Quote.Parser
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either
import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.IO as DTI
import Debug.Trace
import System.IO
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import qualified Text.Parsec.Text as P
import qualified Text.ParserCombinators.Parsec as PC
import Text.PrettyPrint.Mainland (Pretty, ppr, prettyLazyText)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  swifts <- findByExtension [".swift"] "tests/golden"
  defaultMain $ testGroup "Tests "
    [ operatorTests
    , src2ast
    , src2ast2src
    , swifts2Goldens swifts
    ]

swifts2Goldens :: [FilePath] -> TestTree
swifts2Goldens paths = testGroup "Goldens" $ map swift2Golden paths
  where
    swift2Golden s = mkTest s (s <.> "golden")
    mkTest s g = goldenVsStringDiff (dropExtension s) diffCmd g (prettyFile s)
    diffCmd ref new = ["diff", "--unified=5", ref, new]

asdf :: T.Text -> String
asdf input = case parse input of
  (Left err) -> err
  (Right module_) -> L.unpack $ prettyPrint (trace ("\n\n\n" ++ show module_ ++ "\n\n\n") module_)

prettyFile :: String -> IO C.ByteString
prettyFile fileName = do
  contents <- DTI.readFile fileName
  return (C.pack (asdf contents))

litIntExp :: Integer -> Expression
litIntExp i = litExp (IntegerLiteral i)

parserToEither :: P.Parser a -> T.Text -> Either String a
parserToEither p input = left show (PC.parse p "<stdin>" input)

goodOperator :: String -> T.Text -> TestTree
goodOperator o input = testCase ("good operator: " ++ wrap o ++ " input " ++ wrap (T.unpack input)) $
  parserToEither (op o) input @?= Right ()

badOperator :: String -> T.Text -> TestTree
badOperator o input = testCase ("bad operator: " ++ wrap o ++ " input " ++ wrap (T.unpack input)) $
  assertBool ("Expected left, got " ++ show e) (isLeft e)
    where
      e = parserToEither (op o) input

operatorTests :: TestTree
operatorTests = testGroup "Operators"
  [ goodOperator "&"    "&"
  , goodOperator "&"    "& "
  , goodOperator "&"    " &"
  , goodOperator "&"    " & "
  , goodOperator "<"    "<"
  , goodOperator ">"    ">"
  , goodOperator ">"    ">"
  , goodOperator "+"    "+"
  , goodOperator "-"    "-"
  , goodOperator "*"    "*"
  , goodOperator "/"    "/"
  , goodOperator "!"    "!"
  , goodOperator "?"    "?"
  , goodOperator "++"   "++"
  , goodOperator "->"   "->"

  , badOperator "="     "="
  , badOperator  ":"    ":"
  , badOperator  "."    "."
  , badOperator  ".."   ".."
  , badOperator  "..."  "..."
  , badOperator  "_"    "_"
  , badOperator  ","    ","
  , badOperator  "("    "("
  , badOperator  ")"    ")"
  , badOperator  "a"    "a"
  , badOperator  "foo"  "foo"
  ]

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
  , expressionTest "a.123" $ Expression Nothing (PrefixExpression Nothing (ExplicitMemberExpressionDigits (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))) "123")) []
  , expressionTest "a.b" $ Expression Nothing (PrefixExpression Nothing (ExplicitMemberExpressionIdentifier (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))) (IdG {idgIdentifier = "b", idgGenericArgs = []}))) []
  , expressionTest "foo" $ primary1 "foo"
  , expressionTest "a" $ primary1 "a"
  , expressionTest "a1" $ primary1 "a1"
  , expressionTest "xs" $ primary1 "xs"
  , expressionTest "1 is Int" $ typeCastExp (IntegerLiteral 1) "is" (Type "Int")
  , expressionTest "200 as Double" $ typeCastExp (IntegerLiteral 200) "as" (Type "Double")
  , expressionTest "\"s\" as? String" $ typeCastExp (StringLiteral "s") "as?" (Type "String")
  , expressionTest "\"s\" as! String" $ typeCastExp (StringLiteral "s") "as!" (Type "String")
  , expressionTest "a++" $ Expression Nothing (PrefixExpression Nothing
      (PostfixOperator (PostfixPrimary (PrimaryExpression1
        (IdG {idgIdentifier = "a", idgGenericArgs = []}))) "++")) []
  , expressionTest "foo()" $ fooEmptyFunCall
  , expressionTest "foo(1)" $ Expression Nothing (PrefixExpression Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = []}))) [ExpressionElement Nothing (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (IntegerLiteral 1))))) [])] Nothing))) []
  , expressionTest "foo(1, 2)" $ Expression Nothing (PrefixExpression Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = []}))) [ExpressionElement Nothing (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (IntegerLiteral 1))))) []),ExpressionElement Nothing (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (IntegerLiteral 2))))) [])] Nothing))) []
  , expressionTest "foo(1, 2, isBlue: false)" $ Expression Nothing (PrefixExpression Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = []}))) [ExpressionElement Nothing (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (IntegerLiteral 1))))) []),ExpressionElement Nothing (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (IntegerLiteral 2))))) []),ExpressionElement (Just "isBlue") (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (BooleanLiteral False))))) [])] Nothing))) []
  , expressionTest "1.init" $ Expression Nothing (PrefixExpression Nothing (PostfixExpression4Initalizer (PostfixPrimary (PrimaryLiteral (RegularLiteral (IntegerLiteral 1)))))) []
  , expressionTest "a.init" $ Expression Nothing (PrefixExpression Nothing (PostfixExpression4Initalizer (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))))) []
  , expressionTest "foo.init" $ Expression Nothing (PrefixExpression Nothing (PostfixExpression4Initalizer (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = []}))))) []
  , expressionTest "a.self" $ Expression Nothing (PrefixExpression Nothing (PostfixSelf (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))))) []
  , expressionTest "a.dynamicType" $ Expression Nothing (PrefixExpression Nothing (PostfixDynamicType (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))))) []
  , expressionTest "a!" $ Expression Nothing (PrefixExpression Nothing (PostfixForcedValue (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))))) []
  , expressionTest "a?" $ Expression Nothing (PrefixExpression Nothing (PostfixOptionChaining (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))))) []
  , expressionTest "a[1]" $ Expression Nothing (PrefixExpression Nothing (Subscript (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))) [Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (IntegerLiteral 1))))) []])) []

  , moduleTest "import foo" $ singleImport Nothing ["foo"]
  , moduleTest "import foo.math.BitVector" $ singleImport Nothing ["foo", "math", "BitVector"]
  , moduleTest "import typealias foo.a.b" $ singleImport (Just "typealias") ["foo", "a", "b"]

  , moduleTest "print(\"Hello world\\n\")" $ Module [ExpressionStatement (Expression Nothing (PrefixExpression Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "print", idgGenericArgs = []}))) [ExpressionElement Nothing (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (StringLiteral "Hello world\n"))))) [])] Nothing))) [])]

  , moduleTest "let n = 1" $ Module [DeclarationStatement (ConstantDeclaration [] [] [PatternInitializer (ExpressionPattern (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "n", idgGenericArgs = []})))) [BinaryAssignmentExpression {beTryOperator = Nothing, bePrefixExpression = PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (IntegerLiteral 1))))}])) Nothing])]
  , moduleTest "var d = 1.0" $ Module [DeclarationStatement (DeclVariableDeclaration (SimpleVariableDeclaration [PatternInitializer (ExpressionPattern (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "d", idgGenericArgs = []})))) [BinaryAssignmentExpression {beTryOperator = Nothing, bePrefixExpression = PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (FloatingPointLiteral 1.0))))}])) Nothing]))]
  , moduleTest "typealias TypeAliasName = String" $ Module [DeclarationStatement (TypeAlias [] Nothing "TypeAliasName" (Type "String"))]
  ]

emptyModule = Module []

singleImport optImportKind imports = Module [DeclarationStatement (import_ optImportKind (map ImportIdentifier imports))]

fooEmptyFunCall = (Expression Nothing (PrefixExpression Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = []}))) [] Nothing))) [])

initTest1 :: PostfixExpression
initTest1 = PostfixExpression4Initalizer (PostfixPrimary (PrimaryLiteral (RegularLiteral (IntegerLiteral 1))))

initTest2 :: PostfixExpression
initTest2 = PostfixExpression4Initalizer (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = []})))

import_ :: Maybe ImportKind -> ImportPath -> Declaration
import_ = ImportDeclaration []

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
    (PrefixExpression Nothing
      (PostfixPrimary
        (PrimaryExpression1 (IdG identifier [])))) []

typeCastExp :: Literal -> String -> Type -> Expression
typeCastExp lit typeCastKind type_ =
  Expression Nothing
    (PrefixExpression Nothing
      (PostfixPrimary
        (PrimaryLiteral
          (RegularLiteral lit)))) [BinaryExpression4 typeCastKind type_]

litExp :: Literal -> Expression
litExp lit =
  Expression Nothing
    (PrefixExpression Nothing
      (PostfixPrimary
        (PrimaryLiteral
          (RegularLiteral lit)))) []

self :: SelfExpression -> Expression
self se =
  Expression Nothing
    (PrefixExpression Nothing
      (PostfixPrimary
        (PrimarySelf
          se))) []

wrap :: String -> String
wrap s = "[[" ++ s ++ "]]"

moduleTest :: T.Text -> Module -> TestTree
moduleTest input module_ = testCase ("module: " ++ wrap (T.unpack input)) $
  parse input @?= Right module_

expressionTest :: T.Text -> Expression -> TestTree
expressionTest input expression = testCase ("expression: " ++ wrap (T.unpack input)) $
  parseExpression input @?= Right expression

pp :: Pretty pretty => Either d pretty -> Either d L.Text
pp = right (prettyLazyText 100 . ppr)

ppExpTest :: T.Text -> String -> TestTree
ppExpTest input s = testCase ("expression " ++ wrap (T.unpack input) ++ " => " ++ wrap s) $
  sosrc @?= Right s
    where ast = parseExpression input
          osrc = pp ast
          sosrc = fmap L.unpack osrc

m1 :: Module
m1 = Module
  [ExpressionStatement (Expression Nothing
  (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (IntegerLiteral 1)))))
   [])
  ,ExpressionStatement (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (IntegerLiteral 2))))) [])]

e1 :: PrefixExpression
e1 = PrefixExpression Nothing (PostfixOperator (PostfixPrimary (PrimaryLiteral (RegularLiteral (IntegerLiteral 1)))) "*")
