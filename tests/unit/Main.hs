{-# LANGUAGE OverloadedStrings  #-}

import System.Exit

import Language.Swift.Quote.Parser
import Language.Swift.Quote.Syntax
import Language.Swift.Quote.Pretty
import qualified Data.Text.Lazy as L

t input expectedModule = case parse input of
  Right m ->
    if m == expectedModule
    then "SUCCESS " ++ show expectedModule ++ p
    else "Wrong module " ++ show m ++ p
    where
      p = "\n Pretty print:\n" ++ L.unpack (prettyPrint m) ++ "\n"
  Left msg -> "Error parsing module: " ++ msg

p1 = t "1" $ Module $ IntegerLiteral 1
p2 = t "2" $ Module $ IntegerLiteral 2
p3 = t "3" $ Module $ IntegerLiteral 2

s1  = t "\"Hello\"" $ Module $ StringLiteral "Hello"
s2  = t "foo" $ Module $ StringLiteral "bar"
s3  = t "\"foo\"" $ Module $ StringLiteral "\"foo\""

b1 = t "true" $ Module $ BooleanLiteral True
b2 = t " true" $ Module $ BooleanLiteral True
b3 = t " true " $ Module $ BooleanLiteral True
b4 = t " true  " $ Module $ BooleanLiteral True
b5 = t " false" $ Module $ BooleanLiteral False

-- TODO: HUnit/HSpec.
main :: IO ()
main = do
  putStrLn "\nTests:"
  mapM_ (\s -> do putStrLn "\n"; putStrLn s)
    [p1, p2, p3, s1, s2, s3, b1, b2, b3, b4, b5]
  exitSuccess
