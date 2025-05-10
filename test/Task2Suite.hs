{-# OPTIONS_GHC -Wno-orphans #-}
module Task2Suite where

import Test.Tasty
import Test.Tasty.QuickCheck

import Task2 (Date(..), Day(Day), Month(Month), Year(Year), date)
import Parser (parseMaybe)
import Data.List (intercalate)
import Data.Char (isDigit, toLower)

task2Tests :: TestTree
task2Tests = testGroup "Task2"
  [ testProperty "date" $
      withMaxSuccess 1000 $
        \(Blind (x, junk, Format format)) ->
          let str = format x ++ rest
              rest = dropWhile isDigit junk
          in  counterexample ("unexpected result of parseMaybe date " ++ show str) $
            within (milli 100) $
              parseMaybe date str === Just x
  , testProperty "invalid" $
      withMaxSuccess 1000 $
        \(Blind ((d, m, y), junk, BadFormat format)) -> not (validDate d m y) ==>
          let str = format d m y ++ junk
          in  counterexample ("unexpected result of parseMaybe date " ++ show str) $
            within (milli 100) $
              parseMaybe date str === Nothing
  ]

milli :: Int -> Int
milli n = n * 10 ^ (3 :: Int)

instance Arbitrary Date where
  arbitrary = do
    d <- chooseInt (1, 31)
    m <- chooseInt (1, 12)
    y <- chooseInt (0, maxBound)
    pure $ Date (Day d) (Month m) (Year y)

  shrink (Date d m (Year y)) = Date d m . Year <$> shrink y

newtype Format = Format (Date -> String)

instance Arbitrary Format where
  arbitrary = elements $ fmap Format [dotFormat, hyphenFormat, usFormat]

dotFormat :: Date -> String
dotFormat (Date (Day d) (Month m) (Year y)) = intercalate "." [zeroExtended d, zeroExtended m, show y]

hyphenFormat :: Date -> String
hyphenFormat (Date (Day d) (Month m) (Year y)) = intercalate "-" [zeroExtended d, zeroExtended m, show y]

usFormat :: Date -> String
usFormat (Date (Day d) (Month m) (Year y)) = unwords [monthName m, show d, show y]

zeroExtended :: Int -> String
zeroExtended n
  | 0 < n && n < 10 = "0" ++ show n
  | otherwise = show n

monthName :: Int -> String
monthName n = months !! (n - 1)

months :: [String]
months =
  [ "Jan"
  , "Feb"
  , "Mar"
  , "Apr"
  , "May"
  , "Jun"
  , "Jul"
  , "Aug"
  , "Sep"
  , "Oct"
  , "Nov"
  , "Dec"
  ]

-- Note: 1 to 9 values are invalid in terms of "bad" formats below,
-- since they do not pad with extra zero.
validDate :: Int -> Int -> Int -> Bool
validDate d m y = (10 <= d && d <= 31) && (10 <= m && m <= 12) && (0 <= y)

newtype BadFormat = BadFormat (Int -> Int -> Int -> String)

instance Arbitrary BadFormat where
  arbitrary = elements $ fmap BadFormat [badDotFormat, badHyphenFormat, badUsFormat]

badDotFormat :: Int -> Int -> Int -> String
badDotFormat d m y = intercalate "." [show d, show m, show y]

badHyphenFormat :: Int -> Int -> Int -> String
badHyphenFormat d m y = intercalate "-" [show d, show m, show y]

badUsFormat :: Int -> Int -> Int -> String
badUsFormat d m y = unwords [badMonthName m, zeroExtended d, show y]

badMonthName :: Int -> String
badMonthName n = mix $ months !! (n `mod` 12)
  where
    mix
      | 10 <= n && n <= 12 = id
      | n > 0 = take 2
      | otherwise = map toLower

