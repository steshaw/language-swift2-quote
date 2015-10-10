{-# LANGUAGE OverloadedStrings  #-}

import System.Exit

import Language.Swift.Quote.Parser
import Language.Swift.Quote.Syntax
import Language.Swift.Quote.Pretty
import qualified Data.Text.Lazy as L
import qualified Data.Text as T

t input expectedModule = "\ninput = " ++ T.unpack input ++ " " ++
  (case parse input of
    Right m ->
      if m == expectedModule
      then "SUCCESS " ++ show expectedModule ++ p
      else "Wrong module " ++ show m ++ p
      where
        p = "\n Pretty print:\n" ++ L.unpack (prettyPrint m) ++ "\n"
    Left msg -> "Error parsing module: " ++ msg)

litMod lit = Module
  (Expression1 Nothing
    (PeRegular Nothing
      (PrimaryExpression1
        (RegularLiteral lit))) (Just []))

p1 = t "1" $ litMod $ IntegerLiteral 1
p2 = t "2 " $ litMod $ IntegerLiteral 2
p3 = t " 3" $ litMod $ IntegerLiteral 2

s1  = t "\"Hello\"" $ litMod $ StringLiteral "Hello"
s2  = t "foo" $ litMod $ StringLiteral "bar"
s3  = t "\"foo\"" $ litMod $ StringLiteral "foo"

b1 = t "true" $ litMod $ BooleanLiteral True
b2 = t "false" $ litMod $ BooleanLiteral False
b3 = t " true" $ litMod $ BooleanLiteral True
b4 = t " true " $ litMod $ BooleanLiteral True
b5 = t " true  " $ litMod $ BooleanLiteral True
b6 = t " false" $ litMod $ BooleanLiteral False

-- TODO: HUnit/HSpec.
main :: IO ()
main = do
  putStrLn "\nTests:"
  mapM_ putStrLn [p1, p2, p3, s1, s2, s3, b1, b2, b3, b4, b5]
  exitSuccess
