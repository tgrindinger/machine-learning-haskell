module InputProcessor where

import Utils
import Data.List
import Data.Either
import Data.Text (Text, pack, unpack)
import Data.Text.Read
import FieldProcessor
import qualified FieldProcessor as FP

data InputProcessor = InputProcessor { fieldProcessors :: [FieldProcessor],
                                       classIndex      :: Int }

construct :: [[Text]] -> Maybe Int -> InputProcessor
construct newData newClassIndex = InputProcessor newFieldProcessors chosenClass
  where chosenClass        = actualClass newClassIndex $ suggestClass newData
        suggestedTypes     = suggestTypes newData
        adjustedTypes      = adjustTypes suggestedTypes chosenClass
        newFieldProcessors = zipWith FP.construct adjustedTypes (transpose newData)

actualClass :: Maybe Int -> Int -> Int
actualClass Nothing suggestedIndex = suggestedIndex
actualClass (Just newClassIndex) _ = newClassIndex

adjustTypes :: [Text] -> Int -> [Text]
adjustTypes oldTypes newClassIndex = newTypes
  where newTypes = [newType i | i <- [0..length oldTypes - 1]]
        newType i
          | i == newClassIndex = pack "Class"
          | otherwise          = oldTypes !! i

processData :: InputProcessor -> [[Text]] -> [[Double]]
processData processor = fmap (processRecord processor)

processRecord :: InputProcessor -> [Text] -> [Double]
processRecord processor record = classValue : processSample processor unclassedRecord
  where newClassIndex   = classIndex processor
        classValue      = head . FP.processData (fieldProcessors processor !! newClassIndex) $ record !! newClassIndex
        unclassedRecord = take newClassIndex record ++ drop (newClassIndex + 1) record

processSample :: InputProcessor -> [Text] -> [Double]
processSample processor sample = concatMap (uncurry FP.processData) $ zip unclassedProcs sample
  where splitProcs     = splitAt (classIndex processor) $ fieldProcessors processor
        unclassedProcs = fst splitProcs ++ tail (snd splitProcs)

evaluateData :: InputProcessor -> [[Text]] -> Text
evaluateData processor newData = pack $ intercalate "\n" newLines
  where newLines    = ["Number of records: " ++ show (length newData),
                       "Number of columns: " ++ show (length $ head newData),
                       "Attribute ranges:",
                       intercalate "\n" ranges,
                       "Suggested class: " ++ show (classIndex processor),
                       "Suggested types:",
                       intercalate "\n" (unpack <$> types)]
        ranges      = (showRange . attrRange) <$> transpose newData
        showRange p = "(" ++ unpack (fst p) ++ ", " ++ unpack (snd p) ++ ")"
        types       = showType <$> fieldProcessors processor

suggestTypes :: [[Text]] -> [Text]
suggestTypes newData = suggestType <$> transpose newData

suggestType :: [Text] -> Text
suggestType attr
  | not (numCapable attr) = pack "Category"
  | otherwise                            = pack "Distribution"
  where manyRepeats = length (nub attr) <= quot (length attr) 2

suggestClass :: [[Text]] -> Int
suggestClass newData
  | null uniqueCounts                                  = 0
  | included head 0         && count head < count last = 0
  | included last lastIndex && count last < count head = lastIndex
  | otherwise                                          = fst . head $ sortOn snd uniqueCounts
  where attrs        = transpose newData
        recordCount  = length $ head attrs
        validCount p = snd p > 1 && snd p < recordCount
        lastIndex    = length attrs - 1
        uniqueCounts = filter validCount . zip [0..lastIndex] $ (length . nub) <$> attrs
        count f      = snd $ f uniqueCounts
        included f i = fst (f uniqueCounts) == i

attrRange :: [Text] -> (Text, Text)
attrRange attr
  | numCapable attr = (findBound minimum, findBound maximum)
  | otherwise       = (minimum nonMissAttr, maximum nonMissAttr)
  where nonMissAttr = nonMissing attr
        numAttr     = (convertToDouble <$> nonMissAttr) :: [Double]
        findBound f = pack . show $ f numAttr

numCapable :: [Text] -> Bool
numCapable fieldValues = all (isRight . double) $ nonMissing fieldValues

