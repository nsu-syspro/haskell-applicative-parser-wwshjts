{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
-- The above pragma enables all warnings

module Task3 where

import Parser
import ParserCombinators
import Data.Char ( toLower, isDigit )
import Data.List (intercalate)
import Control.Applicative (Alternative(some, many, (<|>)))
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

-- JValue
jValue :: Parser JValue
jValue = choice [jString, jNumber, jBool, jNull, jArray]

-- Whitespace
whitespace :: Parser ()
whitespace = () <$ many (anyOf [' ', '\t', '\r', '\n'])

-- Null
jNull :: Parser JValue
jNull = JNull <$ string "null"

-- Booleans
jTrue :: Parser JValue
jTrue = JBool True <$ string "true"

jFalse :: Parser JValue
jFalse = JBool False <$ string "false"

jBool :: Parser JValue
jBool = jTrue <|> jFalse

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

quotedString :: Parser String
quotedString = 
    let
        quote = char '\"'
    in (quote *> many (choice [unEscapedChar, escapedChar, unicodeChar])) <* quote


jString :: Parser JValue
jString =
    let
        quote = char '\"'
        quotedString = (quote *> many (choice [unEscapedChar, escapedChar, unicodeChar])) <* quote
    in JString <$> quotedString

-- Numbers (-_-;)

signPart :: Parser String
signPart = option "" (string "-")

nonZeroDigit :: Parser Char
nonZeroDigit = satisfy (\ch -> isDigit ch && ch /= '0')

zeroDigit :: Parser Char
zeroDigit = satisfy ('0' ==)

digit :: Parser Char
digit = choice [zeroDigit, nonZeroDigit]

intPart :: Parser String
intPart =
    let
        nonZeroInt = nonZeroDigit `addTo` many digit
        zeroInt    = string "0"
    in zeroInt <|> nonZeroInt

fractionalPart :: Parser String
fractionalPart = char '.' `addTo` some digit

exponentPart :: Parser String
exponentPart =
    let
        e       = char 'e' <|> char 'E'
        sign    = option "" (string "-" <|> string "+")
        convert ((ex, l), r) = [ex] ++ l ++ r
    in convert <$> e `andThenT` sign `andThenT` some digit

jNumber :: Parser JValue
jNumber =
    let
        convert (((sign, int), frac), e) = JNumber $ read (sign ++ int ++ frac ++ e)
    in convert <$> signPart `andThenT` intPart `andThenT`
            option "" fractionalPart `andThenT` option "" exponentPart

-- Arrays

element :: Parser JValue
element  = whitespace *> jValue <* whitespace

jArray :: Parser JValue
jArray =
    let
        lb       = string "["
        rb       = string "]"
        comma    = string ","
        elements = element `addTo` many (comma *> element)
        wh    = [] <$ (lb *> whitespace <* rb)
    in JArray <$> ((lb *> elements <* rb) <|> wh)

jObject :: Parser JValue
jObject =
    let
        lb = string "{"
        rb = string "}"
        comma  = string ","
        column = string ":"
        member = ((whitespace *> quotedString <* whitespace) <* column) `andThenT` element
        members = member `addTo` many (comma *> member)
        wh      = [] <$ (lb *> whitespace <* rb) 
    in JObject <$> ((lb *> members  <* rb) <|> wh) 
