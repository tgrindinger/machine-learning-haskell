module CrossValidatorTest where

import Test.HUnit
import Data.Text (pack)
import Data.HashMap.Strict (HashMap, fromList)
import CrossValidator

testCrossValidator = do
  putStrLn "Testing CrossValidator..."
  runTestTT crossValidatorTests

crossValidatorTests = TestList [TestLabel "test splitData"    testSplitData,
                                TestLabel "test splitData'"   testSplitData',
                                TestLabel "test validateFold" testValidateFold]

testClasses = fromList [(0, [[1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5, 6]]),
                        (1, [[5, 6, 7], [6, 7, 8], [7, 8, 9], [8, 9, 0]])]

-- TODO: this test is broken
testValidateFold = TestCase (do
  assertEqual "validateFold train" 0.5 $ validateFold testClasses 1 2
  )

testSplitData = TestCase (do
  let (train, test) = splitData testClasses 1 2
  assertEqual "splitData train" [[0, 1, 2, 3], [0, 2, 3, 4], [1, 5, 6, 7], [1, 6, 7, 8]] train
  assertEqual "splitData test"  [[0, 3, 4, 5], [0, 4, 5, 6], [1, 7, 8, 9], [1, 8, 9, 0]] test
  )

testSplitData' = TestCase (do
  let (train, test) = splitData' 1 2 [[1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5, 6]]
  assertEqual "splitData' train" [[1, 2, 3], [2, 3, 4]] train
  assertEqual "splitData' test"  [[3, 4, 5], [4, 5, 6]] test
  )

