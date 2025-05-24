{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
-- The above pragma enables all warnings

module Task3 where

import Parser
import ParserCombinators
import Data.Char (toLower)
import Data.List (intercalate)
import Control.Applicative (Alternative(some))
import GHC.Unicode (isHexDigit)
import GHC.Char (chr)
import Numeric (readHex)

-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)

-- | Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
--
json :: Parser JValue
json = error "TODO: define json"

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file

-- Grammar

-- Whitespace
whitespace :: Parser String
whitespace = some $ anyOf [' ', '\t', '\r', '\n']

-- Null
null :: Parser JValue
null = JNull <$ string "null"

-- Booleans
jTrue :: Parser JValue
jTrue = JBool True <$ string "true"

jFalse :: Parser JValue
jFalse = JBool False <$ string "false"

bool :: Parser JValue
bool = choice [jTrue, jFalse] 

-- Strings
unEscapedChar :: Parser Char
unEscapedChar = satisfy (\ch -> ch /= '\\' && ch /= '\"')

escapedChar :: Parser Char
escapedChar =
    let
        rules = 
            [
              ("\\\"", '\"') -- quotation mark
             ,("\\\\", '\\') -- reverse solidus
             ,("\\/" , '/' ) -- solidus (snake)
             ,("\\b" , '\b') -- backspace
             ,("\\f" , '\f') -- formfeed
             ,("\\n" , '\n') -- linefeed
             ,("\\r" , '\r') -- cariage return
             ,("\\t" , '\t') -- horizontal tab
            ]
    in choice [ chr' <$ string str | (str, chr') <- rules] 

unicodeChar :: Parser Char 
unicodeChar = string "\\u" *> (hexToChar <$> someN 4 (satisfy isHexDigit) )
    where hexToChar inp = case readHex inp of 
            [(codePoint, _)] -> chr codePoint
            _                -> undefined

