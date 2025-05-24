{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}
-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note
    -- 
    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser
  , parse
  , parseMaybe
  , satisfy
  , label
  , (<?>)
  , Error(..)
  , Position(..)
  , Parsed(..)
  , Input
  ) where

import Control.Applicative

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
 deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | Labeled String  -- ^ Error with an extra information provided
  | EndOfInput      -- ^ Unexpected end of input
 deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
 deriving Show

instance Functor Parsed where
    fmap f (Parsed p rest) = Parsed (f p) rest
    fmap _ (Failed l)      = Failed l

instance Applicative Parsed where
    pure value = Parsed value (Position 0 "")

    (Failed err) <*> _ = Failed err
    (Parsed f _) <*> r = fmap f r

instance Alternative Parsed where
    empty = Failed []

    Parsed x rest <|> _                = Parsed x rest
    Failed _      <|> p@(Parsed _ _)   = p
    Failed fail1  <|> Failed fail2     = Failed $ deduplicate (fail1 <> fail2) -- kinda expensive
        where
            deduplicate []     = []
            deduplicate (x:xs) = x : deduplicate (filter (/= x) xs)

-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse p s = runParser p $ Position 0 s

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe parser str = case parse parser str of
    Parsed x _ -> Just x
    Failed _   -> Nothing

instance Functor Parser where
    fmap f (Parser i2p) = Parser (fmap f . i2p)

instance Applicative Parser where
    -- Parsed :: a -> (Input -> Parsed a Input)
    -- Parsed a :: (Input -> Parsed a Input) <-- looks like parser
    -- | so it is parser that accepts given argument regardless of input
    -- >>> parse (pure 42) "The parser of things is a function from strings ..."
    -- Parsed 42 (Position 0 "The parser of things is a function from strings ...")
    pure = Parser . Parsed

    (Parser fa2b) <*> (Parser fa) = Parser $ \inp -> case fa2b inp of
        Parsed f rest    -> case fa rest of
            Parsed x rest'  -> Parsed (f x) rest'
            Failed lerr     -> Failed lerr 
        Failed lerr   -> Failed lerr 

instance Alternative Parser where
    empty = Parser $ const empty

    -- Note: when both parsers fail, their errors are accumulated and *deduplicated* to simplify debugging
    (<|>) (Parser pa) (Parser pb) = Parser $ \inp -> pa inp <|> pb inp

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
    Position p (x : xs) -> if predicate x
        then Parsed x (Position (succ p) xs) -- I feel so haskellish when use 'succ' instead of +1
        else Failed [Position p $ Unexpected x]
    Position p [] -> Failed [Position p EndOfInput]


-- | Label combinator 
-- Parser can be labeled to provide more useful errors
-- This combinator can be used to build powerful
-- error analysis system, but it is another story. 
-- In current version it is mostly useless and even harmful
label :: String -> Parser a -> Parser a
label l (Parser pa) = Parser $ \inp@(Position pos _) -> case pa inp of
        Parsed x rest -> Parsed x rest
        Failed err    -> Failed (Position pos (Labeled l) : err) where

(<?>) :: Parser a -> String -> Parser a
(<?>) = flip label
