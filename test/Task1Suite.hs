module Task1Suite where

import Test.Tasty
import Test.Tasty.QuickCheck

import Task1 (nat, int)
import Parser (parseMaybe)
import Data.Char (isDigit)


task1Tests :: TestTree
task1Tests = testGroup "Task1"
  [ testProperty "nat" $
      withMaxSuccess 1000 $
        \(Blind (Positive x, junk)) ->
          let str = show x ++ rest
              rest = dropWhile isDigit junk 
          in  counterexample ("unexpected result of parseMaybe nat " ++ show str) $
            within (milli 100) $
              parseMaybe nat str === Just x
  , testProperty "nat (parsing error)" $
      withMaxSuccess 1000 $
        \(Blind junk) ->
          let str = rest
              rest = dropWhile isDigit junk 
          in  counterexample ("unexpected result of parseMaybe nat " ++ show str) $
            within (milli 100) $
              classify (null str) "empty" $
              parseMaybe nat str === Nothing
  , testProperty "int" $
      withMaxSuccess 1000 $
        \(Blind (x, junk)) ->
          let str = show x ++ rest
              rest = dropWhile isDigit junk 
          in  counterexample ("unexpected result of parseMaybe int " ++ show str) $
            within (milli 100) $
              parseMaybe int str === Just x
  , testProperty "int (parsing error)" $
      withMaxSuccess 1000 $
        \(Blind junk) ->
          let str = rest
              rest = dropWhile (\c -> isDigit c || c == '-') junk 
          in  counterexample ("unexpected result of parseMaybe int " ++ show str) $
            within (milli 100) $
              classify (null str) "empty" $
                parseMaybe int str === Nothing
  ]

milli :: Int -> Int
milli n = n * 10 ^ (3 :: Int)

