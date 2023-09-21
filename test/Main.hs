module Main where

import Hedgehog (property, test, withTests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Jannie.Test.NameTest (test_name)

tests :: TestTree
tests =
  testGroup "all" [
    testProperty "name" (withTests 1 (property (test test_name)))
  ]

main :: IO ()
main = defaultMain tests
