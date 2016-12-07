module NaiveBayesTest where

import Utils
import System.IO
import Data.List
import Test.HUnit
import NaiveBayes
import Data.HashMap.Strict (HashMap, toList)
import Test.QuickCheck

testNaiveBayes = do
  quickCheckResult prop_partitionData
  quickCheckResult prop_getMeanAndVariance

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

-- test getMeanAndVariance
newtype StatData = StatData   { unStatData  ::  [Double]  } deriving (Show, Eq)

instance Arbitrary StatData where
  arbitrary = do
    size <- choose (2,100)
    newData <- vectorOf size arbitrary
    return $ StatData newData

getMeanAndVariance' :: StatData -> (Double, Double)
getMeanAndVariance' = getMeanAndVariance . unStatData

prop_getMeanAndVariance statData = result
  where p         = getMeanAndVariance' statData
        list      = unStatData statData
        pShifted  = getMeanAndVariance' . StatData $ (10 +) <$> list
        pScaled   = getMeanAndVariance' . StatData $ (10 *) <$> list
        cond      = fst p >= minimum list && fst p <= maximum list && snd p >= 0
          && closeTo (fst pShifted) (fst p + 10) && closeTo (snd pShifted) (snd p)
          && closeTo (fst pScaled) (fst p * 10) && closeTo (snd pScaled) (snd p * 100)
        collected = collect (show $ 10 * mod (length list) 10) cond
        countered = counterexample ("counter:\n" ++ "p: " ++ show p ++ "\n" ++ "pShifted: " ++ show pShifted ++ "\n" ++ "pScaled: " ++ show pScaled ++ "\n" ++ show list) cond
        result = if cond then collected else countered

