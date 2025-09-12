module Main (main) where

import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

-- Import the function to be tested
import BinarySearch.BinarySearch (search, searchRec)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "BinarySearch"
        [ testCase "Case 1" $
            let nums = V.fromList [-1, 0, 2, 4, 6, 8]
                target = 4
                expected = 3
                actual = search nums target
             in assertEqual "Should return the correct index for an existing element" expected actual
        , testCase "Case 2" $
            let nums = V.fromList [-1, 0, 2, 4, 6, 8]
                target = 3
                expected = -1
                actual = search nums target
             in assertEqual "Should return -1 for a non-existent element" expected actual
        , testCase "Case 1 (recursive)" $
            let nums = V.fromList [-1, 0, 2, 4, 6, 8]
                target = 4
                expected = 3
                actual = searchRec nums target
             in assertEqual "Should return the correct index for an existing element" expected actual
        , testCase "Case 2 (recursive)" $
            let nums = V.fromList [-1, 0, 2, 4, 6, 8]
                target = 3
                expected = -1
                actual = searchRec nums target
             in assertEqual "Should return -1 for a non-existent element" expected actual
        ]
