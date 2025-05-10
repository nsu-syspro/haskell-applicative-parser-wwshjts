{-# OPTIONS_GHC -Wno-orphans #-}
module Task3Suite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Task3 (JValue(..), json, renderJSONFile, renderTokens)
import Parser (parseMaybe)
import Data.List (isSuffixOf)
import System.Directory (listDirectory)
import Data.Ord (clamp)



task3Tests :: IO TestTree
task3Tests = do
  files <- map ("samples/" ++) . filter (".json" `isSuffixOf`) <$> listDirectory "samples"
  pure $ testGroup "Task3" $
    fmap sampleTest files ++
    [ testProperty "json" $
        withMaxSuccess 1000 $
          \(Blind (x, Paddings paddings)) ->
            let str = concat $ zipWith (++) paddings $ renderTokens x
            in  counterexample ("unexpected result of parseMaybe json " ++ show str) $
              within (milli 100) $
                parseMaybe json str === Just x
    ]

sampleTest :: String -> TestTree
sampleTest file =
  localOption (mkTimeout (seconds 2)) $
    testCase file $ do
      expected <- readFile (file ++ ".expected")
      actual <- renderJSONFile file
      assertEqual file expected actual

milli :: Int -> Int
milli n = n * 10 ^ (3 :: Int)

seconds :: Integer -> Integer
seconds n = n * 10 ^ (6 :: Int)


instance Arbitrary JValue where
  arbitrary = scale (clamp (0, 10)) $ sized arbitrarySizedJValue

arbitrarySizedJValue :: Int -> Gen JValue
arbitrarySizedJValue 0 = oneof
  [ pure JNull
  , JBool <$> arbitrary
  , JNumber <$> arbitrary
  , JString . getJSONString <$> arbitrary
  ]
arbitrarySizedJValue n = oneof
  [ JArray <$> (chooseInt (0, n) >>= \k -> vectorOf k (arbitrarySizedJValue n'))
  , JObject <$> (chooseInt (0, n) >>= \k -> vectorOf k ((,) . getJSONString <$> arbitrary <*> arbitrarySizedJValue n'))
  ]
 where
  n' = n `div` 2

newtype JSONString = JSONString { getJSONString :: String }
  deriving Show

instance Arbitrary JSONString where
  arbitrary = JSONString . concat <$> listOf (elements $
      (pure <$> ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) ++
      (('\\' :) <$> ["\\", "\"", "/", "b", "f", "n", "r", "t"]))

newtype Paddings = Paddings [[Char]]

instance Arbitrary Paddings where
  arbitrary = fmap Paddings $
    infiniteListOf $
    scale (clamp (0, 5)) $ listOf $
    frequency
      [ (60, elements [' '])
      , (30, elements ['\n'])
      , (10, elements ['\t'])
      ]
