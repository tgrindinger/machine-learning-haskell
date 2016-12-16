module NaiveBayes where

import Utils
import Data.Maybe
import Data.List
import Data.HashMap.Strict (HashMap, (!), keys, fromList)
import qualified Data.HashMap.Strict as HM
import Data.Set (Set, notMember)
import qualified Data.Set as S

data NaiveBayes = NaiveBayes { classes    :: [Double],
                               popSize    :: Int,
                               classSizes :: HashMap Double Int,
                               means      :: HashMap Double [Double],
                               variances  :: HashMap Double [Double],
                               zeroVar    :: Set Int }

construct :: [[Double]] -> NaiveBayes
construct newData = NaiveBayes newClasses (length newData) newClassSizes newMeans newVars newZeroVars
  where classData      = partitionData newData
        newClasses     = keys classData
        newClassSizes  = length <$> classData
        muSigmaMap     = fmap getMeanAndVariance . transpose <$> classData
        splitMuSigma f = fmap f <$> muSigmaMap
        newMeans       = splitMuSigma fst
        newVars        = splitMuSigma snd
        newZeroVars    = zeroVarFields newVars

zeroVarFields :: HashMap Double [Double] -> Set Int
zeroVarFields newVars = S.fromList . concatMap zeroVarFields' $ HM.elems newVars

zeroVarFields' :: [Double] -> [Int]
zeroVarFields' list = zeroIndices
  where varStatus = (< 0.1) <$> list
        zeroIndices = fst <$> filter snd (zip [0..] varStatus)

classify :: [Double] -> NaiveBayes -> Double
classify sample naiveBayes = fst . last $ sortOn snd predictions
  where classMap i  = (i, classify' sample naiveBayes i)
        predictions = classMap <$> classes naiveBayes

classify' :: [Double] -> NaiveBayes -> Double -> Double
classify' sample naiveBayes classId = classProb * attrProb
  where classData f = f naiveBayes ! classId
        classProb   = fromIntegral (classData classSizes) / fromIntegral (popSize naiveBayes)
        nonZeroVar list = snd <$> filter (flip notMember (zeroVar naiveBayes) . fst) (zip [0..] list)
        newMeans    = nonZeroVar (classData means)
        newVars     = nonZeroVar (classData variances)
        newSample   = nonZeroVar sample
        attrProb    = product $ zipWith3 getP newSample newMeans newVars

getP :: Double -> Double -> Double -> Double
getP sample mean variance = coeff * exp term
  where term  = negate ((sample - mean) ** 2) / (2 * variance)
        coeff = 1 / sqrt (2 * pi * variance)

getMeanAndVariance :: [Double] -> (Double, Double)
getMeanAndVariance list = (mean, coefficient * variable)
  where mean             = findMean list
        coefficient      = 1 / fromIntegral (length list - 1)
        variable         = foldl' varianceTerm 0 list
        varianceTerm s v = s + ((mean - v) ** 2)

