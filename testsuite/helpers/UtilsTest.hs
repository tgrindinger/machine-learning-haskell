module UtilsTest where

import Utils
import System.IO
import Data.List
import Test.HUnit
import NaiveBayes
import Data.HashMap.Strict (HashMap, toList)
import Test.QuickCheck

testUtils = do
  quickCheckResult prop_partitionData

-- test partitionData
newtype InputData = InputData { unInputData :: [[Double]] } deriving (Show, Eq)

instance Arbitrary InputData where
  arbitrary = do
    size <- choose (1, 50)
    newData <- listOf1 $ vectorOf size arbitrary
    return $ InputData newData

unpartitionData :: HashMap Double [[Double]] -> [[Double]]
unpartitionData hashMap = concatMap (uncurry classedLists) $ toList hashMap
  where classedLists classVal = fmap (classVal :)

partitionData' :: InputData -> HashMap Double [[Double]]
partitionData' = partitionData . unInputData

prop_partitionData inputData = result
  where undone    = unpartitionData $ partitionData' inputData
        cond      = all (flip elem $ unInputData inputData) undone && length (unInputData inputData) == length undone
        collected = collect (show $ 10 * mod (length $ unInputData inputData) 10) cond
        countered = counterexample ("counter:\n" ++ show (partitionData' inputData) ++ "\n" ++ show (unInputData inputData) ++ "\n" ++ show undone) cond
        result = if cond then collected else countered

