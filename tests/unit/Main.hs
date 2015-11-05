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
  defaultMain $ testGroup "Unit"
    [ operatorTests
    , src2ast
    , src2ast2src
    , swifts2Goldens swifts
    ]

swifts2Goldens :: [FilePath] -> TestTree
swifts2Goldens paths = testGroup "Golden" $ map swift2Golden paths
  where
    swift2Golden s = mkTest s (s <.> "golden")
    mkTest s g = goldenVsStringDiff (dropExtension s) diffCmd g (file2ast2bytestring s)
    diffCmd ref new = ["diff", "--unified=5", ref, new]

text2ast2string :: T.Text -> String
text2ast2string input = case parse input of
  (Left err) -> err
  (Right m) -> L.unpack $ prettyPrint (trace ("\n\n" ++ show m ++ "\n\n") m)

file2ast2bytestring :: String -> IO C.ByteString
file2ast2bytestring fileName = do
  contents <- DTI.readFile fileName
  return (C.pack (text2ast2string contents))

litIntExp :: Integer -> Expression
litIntExp i = litExp (NumericLiteral (show i))

litStrExp :: String -> Expression
litStrExp s = litExp (StringLiteral (StaticStringLiteral s))

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
operatorTests = testGroup "Operator"
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
  , goodOperator "="     "="

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
src2ast = testGroup "src2ast"
  [ expressionTest "1" $ litIntExp 1
  , expressionTest "-1" $ Expression Nothing (PrefixExpression (Just "-") (PostfixPrimary (PrimaryLiteral (RegularLiteral (NumericLiteral "1"))))) []
  , expressionTest " 2" $ litIntExp 2
  , expressionTest "3 " $ litIntExp 3
  , expressionTest " 10 " $ litIntExp 10
  , expressionTest "1234567890" $ litIntExp 1234567890
  , expressionTest "1.0" $ litExp (NumericLiteral "1.0")
  , expressionTest "1.1234567890" $ litExp (NumericLiteral "1.1234567890")
  , expressionTest "0xb10101111" $ litExp (NumericLiteral "0xb10101111")
  , expressionTest "0xCAFEBABE" $ litExp (NumericLiteral "0xCAFEBABE")
  , expressionTest "0o12345670" $ litExp (NumericLiteral "0o12345670")
  , expressionTest "\"Hello\"" $ litStrExp "Hello"
  , expressionTest " \"Hello\"" $ litStrExp "Hello"
  , expressionTest "\"Hello\" " $ litStrExp "Hello"
  , expressionTest " \"Hello\" " $ litStrExp "Hello"
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
  , expressionTest "self" $ self Self
  , expressionTest "self.a" $ self (SelfMethod "a")
  , expressionTest "self. a" $ self (SelfMethod "a")
  , expressionTest "self . a" $ self (SelfMethod "a")
  , expressionTest " self . a" $ self (SelfMethod "a")
  , expressionTest " self . a " $ self (SelfMethod "a")
  , expressionTest "self[1]" $ self (SelfSubscript [litIntExp 1])
  , expressionTest "self[1,2]" $ self (SelfSubscript [litIntExp 1, litIntExp 2])
  , expressionTest "self[1, 2]" $ self (SelfSubscript [litIntExp 1, litIntExp 2])
  , expressionTest "self [1, 2]" $ self (SelfSubscript [litIntExp 1, litIntExp 2])
  , expressionTest "self [ 1, 2 ]" $ self (SelfSubscript [litIntExp 1, litIntExp 2])
  , expressionTest "self.init" $ self SelfInit
  , expressionTest "a.123" $ Expression Nothing (PrefixExpression Nothing (ExplicitMemberExpressionDigits (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))) "123")) []
  , expressionTest "a.b" $ Expression Nothing (PrefixExpression Nothing (ExplicitMemberExpressionIdentifier (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))) (IdG {idgIdentifier = "b", idgGenericArgs = []}))) []
  , expressionTest ".b" $ Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryImplicitMember "b"))) []
  , expressionTest "foo" $ primary1 "foo"
  , expressionTest "a" $ primary1 "a"
  , expressionTest "a1" $ primary1 "a1"
  , expressionTest "xs" $ primary1 "xs"
  , expressionTest "1 is Int" $ typeCastExp (NumericLiteral "1") "is" (TypeIdentifierType (TypeIdentifier [("Int",[])]))
  , expressionTest "200 as Double" $ typeCastExp (NumericLiteral "200") "as" (TypeIdentifierType (TypeIdentifier [("Double",[])]))
  , expressionTest "\"s\" as? String" $ typeCastExp (StringLiteral (StaticStringLiteral "s")) "as?" (TypeIdentifierType (TypeIdentifier [("String",[])]))
  , expressionTest "\"s\" as! String" $ typeCastExp (StringLiteral (StaticStringLiteral "s")) "as!" (TypeIdentifierType (TypeIdentifier [("String",[])]))
  , expressionTest "a++" $ Expression Nothing (PrefixExpression Nothing
      (PostfixOperator (PostfixPrimary (PrimaryExpression1
        (IdG {idgIdentifier = "a", idgGenericArgs = []}))) "++")) []
  , expressionTest "foo()" $ (Expression Nothing (PrefixExpression Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = []}))) [] Nothing))) [])
  , expressionTest "foo(1)" $ Expression Nothing (PrefixExpression Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = []}))) [ExpressionElement Nothing (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (NumericLiteral "1"))))) [])] Nothing))) []
  , expressionTest "foo(1, 2)" $ Expression Nothing (PrefixExpression Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = []}))) [ExpressionElement Nothing (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (NumericLiteral "1"))))) []),ExpressionElement Nothing (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (NumericLiteral "2"))))) [])] Nothing))) []
  , expressionTest "foo(1, 2, isBlue: false)" $ Expression Nothing (PrefixExpression Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = []}))) [ExpressionElement Nothing (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (NumericLiteral "1"))))) []),ExpressionElement Nothing (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (NumericLiteral "2"))))) []),ExpressionElement (Just "isBlue") (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (BooleanLiteral False))))) [])] Nothing))) []
  , expressionTest "1.init" $ Expression Nothing (PrefixExpression Nothing (PostfixExpression4Initalizer (PostfixPrimary (PrimaryLiteral (RegularLiteral (NumericLiteral "1")))))) []
  , expressionTest "a.init" $ Expression Nothing (PrefixExpression Nothing (PostfixExpression4Initalizer (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))))) []
  , expressionTest "foo.init" $ Expression Nothing (PrefixExpression Nothing (PostfixExpression4Initalizer (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "foo", idgGenericArgs = []}))))) []
  , expressionTest "a.self" $ Expression Nothing (PrefixExpression Nothing (PostfixSelf (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))))) []
  , expressionTest "a.dynamicType" $ Expression Nothing (PrefixExpression Nothing (PostfixDynamicType (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))))) []
  , expressionTest "a!" $ Expression Nothing (PrefixExpression Nothing (PostfixForcedValue (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))))) []
  , expressionTest "a?" $ Expression Nothing (PrefixExpression Nothing (PostfixOptionChaining (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))))) []
  , expressionTest "a[1]" $ Expression Nothing (PrefixExpression Nothing (Subscript (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "a", idgGenericArgs = []}))) [Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (NumericLiteral "1"))))) []])) []

  , expressionTest "try someThrowingFunction() + anotherThrowingFunction()" $ Expression (Just "try") (PrefixExpression Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "someThrowingFunction", idgGenericArgs = []}))) [] Nothing))) [BinaryExpression1 {beOperator = "+", bePrefixExpression = PrefixExpression Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "anotherThrowingFunction", idgGenericArgs = []}))) [] Nothing))}]
  , expressionTest "(try someThrowingFunction()) + anotherThrowingFunction()" $
      Expression Nothing (PrefixExpression Nothing (PostfixPrimary
       (PrimaryParenthesized
         [ExpressionElement Nothing
           (Expression (Just "try") (PrefixExpression Nothing
             (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "someThrowingFunction", idgGenericArgs = []}))) [] Nothing))) [])
         ]))) [BinaryExpression1 {beOperator = "+", bePrefixExpression = PrefixExpression Nothing
           (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "anotherThrowingFunction", idgGenericArgs = []}))) [] Nothing))}]

  , moduleTest "import foo" $ singleImport Nothing ["foo"]
  , moduleTest "import foo.math.BitVector" $ singleImport Nothing ["foo", "math", "BitVector"]
  , moduleTest "import typealias foo.a.b" $ singleImport (Just "typealias") ["foo", "a", "b"]

  , moduleTest "print(\"Hello world\\n\")" $ Module [ExpressionStatement (Expression Nothing (PrefixExpression Nothing (FunctionCallE (FunctionCall (PostfixPrimary (PrimaryExpression1 (IdG {idgIdentifier = "print", idgGenericArgs = []}))) [ExpressionElement Nothing (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (StringLiteral (StaticStringLiteral"Hello world\n")))))) [])] Nothing))) [])]

  , moduleTest "let n = 1" $ Module [DeclarationStatement (ConstantDeclaration [] [] [PatternInitializer (IdentifierPattern "n" Nothing) (Just (Initializer (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (NumericLiteral "1"))))) [])))])]

  , moduleTest "var d = 1.0" $ Module [DeclarationStatement (DeclVariableDeclaration (VarDeclPattern [] [] [PatternInitializer (IdentifierPattern "d" Nothing) (Just (Initializer (Expression Nothing (PrefixExpression Nothing (PostfixPrimary (PrimaryLiteral (RegularLiteral (NumericLiteral "1.0"))))) [])))]))]

  , moduleTest "typealias TypeAliasName = String" $ Module [DeclarationStatement (TypeAlias [] Nothing "TypeAliasName" (TypeIdentifierType (TypeIdentifier [("String",[])])))]
  ]

singleImport :: Maybe ImportKind -> [String] -> Module
singleImport optImportKind imports = Module [DeclarationStatement (import_ optImportKind (map ImportIdentifier imports))]

import_ :: Maybe ImportKind -> ImportPath -> Declaration
import_ = ImportDeclaration []

src2ast2src :: TestTree
src2ast2src = testGroup "src2ast2src"
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
primary1 ident =
  Expression Nothing
    (PrefixExpression Nothing
      (PostfixPrimary
        (PrimaryExpression1 (IdG ident [])))) []

typeCastExp :: Literal -> String -> Type -> Expression
typeCastExp lit typeCastKind t =
  Expression Nothing
    (PrefixExpression Nothing
      (PostfixPrimary
        (PrimaryLiteral
          (RegularLiteral lit)))) [BinaryExpression4 typeCastKind t]

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
moduleTest input m = testCase ("module: " ++ wrap (T.unpack input)) $
  parse input @?= Right m

expressionTest :: T.Text -> Expression -> TestTree
expressionTest input e = testCase ("expression: " ++ wrap (T.unpack input)) $
  parseExpression input @?= Right e

pp :: Pretty pretty => Either d pretty -> Either d L.Text
pp = right (prettyLazyText 100 . ppr)

ppExpTest :: T.Text -> String -> TestTree
ppExpTest input s = testCase ("expression " ++ wrap (T.unpack input) ++ " => " ++ wrap s) $
  sosrc @?= Right s
    where ast = parseExpression input
          osrc = pp ast
          sosrc = fmap L.unpack osrc
