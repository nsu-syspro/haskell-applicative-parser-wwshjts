{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module ParserCombinators where

import Parser

import Control.Applicative

satisfyNot :: (Char -> Bool) -> Parser Char
satisfyNot predecate = satisfy (not . predecate)

-- | Parses single character
--
-- Usage example:
--
-- >>> parse (char 'b') "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (char 'b') "abc"
-- Failed [Position 0 (Unexpected 'a')]
--
char :: Char -> Parser Char
char ch = satisfy (ch ==)

-- | Parses given string
--
-- Usage example:
--
-- >>> parse (string "ba") "bar"
-- Parsed "ba" (Position 2 "r")
-- >>> parse (string "ba") "abc"
-- Failed [Position 0 (Unexpected 'a')]
--
string :: String -> Parser String
string = traverse char

-- | Skips zero or more space characters
--
-- Usage example:
--
-- >>> parse spaces "  bar"
-- Parsed () (Position 2 "bar")
-- >>> parse spaces "bar"
-- Parsed () (Position 0 "bar")
-- >>> parse (spaces *> string "bar") "bar"
-- Parsed "bar" (Position 3 "")
--
spaces :: Parser ()
spaces = () <$ many (char ' ')

-- | Tries to consecutively apply each of given list of parsers until one succeeds.
-- Returns the *first* succeeding parser as result or 'empty' if all of them failed.
--
-- Usage example:
--
-- >>> parse (choice [char 'a', char 'b']) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (choice [char 'a', char 'b']) "foo"
-- Failed [Position 0 (Unexpected 'f')]
-- >>> parse (choice [string "ba", string "bar"]) "bar"
-- Parsed "ba" (Position 2 "r")
--
choice :: (Foldable t, Alternative f) => t (f a) -> f a
choice = asum

addTo :: Parser a -> Parser [a] -> Parser [a]
addTo = liftA2 (:)

andThen :: Parser a -> Parser a -> Parser [a]
andThen f s = f `addTo` ((: []) <$> s)

andThenT :: Parser a -> Parser b -> Parser (a, b)
andThenT f s = (,) <$> f <*> s 

anyOf :: [Char] -> Parser Char 
anyOf chs = choice $ char <$> chs 

someN :: Int -> Parser a -> Parser [a]
someN 0   _      = pure [] 
someN cnt parser = (:) <$> parser <*> someN (cnt - 1) parser 

option :: a -> Parser a -> Parser a
option x p = p <|> pure x


-- Discover and implement more useful parser combinators below
--
-- - <https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html>
-- - <https://hackage.haskell.org/package/parsec-3.1.18.0/docs/Text-Parsec-Char.html>
