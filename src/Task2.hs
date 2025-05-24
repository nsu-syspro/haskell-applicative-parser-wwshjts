{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser
import ParserCombinators
import Data.Char (isDigit)
import Control.Applicative (Alternative(some))

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--
date :: Parser Date
date = choice [dotFormat, hyphenFormat, usFormat] 

months :: [String]
months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

monthName :: Parser String
monthName = choice $ string <$> months 

index :: Eq a => a -> [a] -> Int
index = index' 0 where 
    index' _ _ []       = undefined 
    index' cnt e (x:xs) = if x == e then cnt else index' (succ cnt) e xs 

monthToInt :: String -> Int
monthToInt s = succ $ index s months

nonZeroDigit :: Parser Char 
nonZeroDigit = satisfy (\ch -> isDigit ch && ch /= '0')

zeroDigit :: Parser Char 
zeroDigit = satisfy ('0' ==)

digit :: Parser Char 
digit = choice [zeroDigit, nonZeroDigit] 

number :: Parser String
number = some digit

year :: Parser Year
year = Year . read <$> number

day :: Parser Day
day = Day . read <$> 
    choice [zeroDigit `andThen` digit, char '1' `andThen` digit, 
    char '2' `andThen` digit, string "30", string "31"] 

usDay :: Parser Day
usDay = Day . read <$>
        choice [char '1' `andThen` digit, char '2' `andThen` digit, 
        string "30", string "31" , (: []) <$> nonZeroDigit] 
                

month :: Parser Month
month = Month . read <$> choice [zeroDigit `andThen` nonZeroDigit, string "10", string "11", string "12"]

usMonth :: Parser Month
usMonth = Month . monthToInt <$> monthName

dateCombiner :: Char -> Parser Day -> Parser Month -> Parser Date 
dateCombiner delimiter dp mp = 
    let
        del = char delimiter
        convert ((((d, _), m), _), y) = Date d m y -- pretty ugly : ( 
    in convert <$> dp `andThenT` del `andThenT` mp `andThenT` del `andThenT` year 

dotFormat :: Parser Date
dotFormat = dateCombiner '.' day month

hyphenFormat :: Parser Date
hyphenFormat = dateCombiner '-' day month

usFormat :: Parser Date
usFormat = let
        del = char ' ' 
        convert ((((m, _), d), _), y) = Date d m y -- pretty ugly : ( 
    in convert <$> usMonth `andThenT` del `andThenT` usDay `andThenT` del `andThenT` year 

