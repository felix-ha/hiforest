import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import Lib


x :: Dataset
x = [[1.2, 1.1], [1.9, 0.2], [2.2, 0.1], [1.9, 0.2]]

testFeatures :: Test
testFeatures = TestCase (assertEqual "numberOfFeatures x" 3 (numberOfFeatures x))

testIsValid :: Test
testIsValid = TestCase (assertEqual "isValidDataset x" True (isValidDataset x))

tests :: Test
tests = TestList
    [ TestLabel "testFeatures" testFeatures
    , TestLabel "testIsValid"  testIsValid
    ]

main :: IO ()
main = do
    counts <- runTestTT tests
    print counts
    if errors counts == 0 && failures counts == 0
      then exitSuccess
      else exitFailure

