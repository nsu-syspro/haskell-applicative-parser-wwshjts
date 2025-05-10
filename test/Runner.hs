import Test.Tasty

import Task1Suite
import Task2Suite
import Task3Suite

main :: IO ()
main = do
  ts <- tests
  defaultMain ts

tests :: IO TestTree
tests = do
  ts3 <- task3Tests
  pure $ testGroup "Tests"
    [ task1Tests
    , task2Tests
    , ts3
    ]
