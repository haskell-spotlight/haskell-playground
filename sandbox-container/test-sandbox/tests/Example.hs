module Main where

import qualified SandboxLib
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests =
  testGroup
    "Unit tests"
    [ testCase "sum" $
        SandboxLib.sum 40 2 @?= 42,
      testCase "everythingWillBeGood" $
        SandboxLib.everythingWillBeGood @?= True,
      testCase "amISupportedHaskellSpot" $
        SandboxLib.amISupportedHaskellSpot @?= True
    ]
