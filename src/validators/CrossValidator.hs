module CrossValidator where

import Utils
import Data.Maybe
import Data.List
import NaiveBayes
import Data.HashMap.Strict (HashMap, keys, elems)
import qualified Data.HashMap.Strict as HM

-- TODO: evaluate this example: data size = 10, fold size = 3, last element will not be used
-- TODO: what if some class has less than numFolds members?
crossValidate :: Int -> [[Double]] -> Double
crossValidate numFolds newData = sum accuracies / fromIntegral numFolds
  where partitioned = partitionData newData
        accuracies  = validateFold partitioned numFolds <$> [0..numFolds - 1]

validateFold :: HashMap Double [[Double]] -> Int -> Int -> Double
validateFold newData numFolds foldNum = numCorrect / fromIntegral (length actuals)
  where (testData, trainData) = splitData newData foldNum numFolds
        nb                    = construct trainData
        predictions           = (flip classify) nb <$> tail <$> testData
        actuals               = head <$> testData
        numCorrect            = foldl' countAccurate 0 $ zip actuals predictions
        countAccurate s (actual, predicted) = s + if predicted == actual then 1 else 0 :: Double

splitData :: HashMap Double [[Double]] -> Int -> Int -> ([[Double]], [[Double]])
splitData hashMap foldNum numFolds = (combineData trainData, combineData testData)
  where splits          = splitData' foldNum numFolds <$> elems hashMap
        trainData       = fst <$> splits
        testData        = snd <$> splits
        combineData d   = concat $ classWithData <$> zip (keys hashMap) d
        classWithData p = (fst p :) <$> snd p

splitData' :: Int -> Int -> [[Double]] -> ([[Double]], [[Double]])
splitData' foldNum numFolds classData = (trainData, testData)
  where foldSize = quot (length classData) $ fromIntegral numFolds
        lowerBound = foldNum * foldSize
        upperBound = min (length classData) (lowerBound + foldSize)
        testData   = take foldSize $ drop lowerBound classData
        trainData  = take lowerBound classData ++ drop upperBound classData

