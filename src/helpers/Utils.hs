module Utils where

import Data.List
import Data.HashMap.Strict (HashMap, fromList)
import Data.Text (Text, unpack)
import Data.Text.Read

convertToDouble :: Text -> Double
convertToDouble text = either (error $ "cannot convert '" ++ unpack text ++ "' to double") fst $ double text

closeTo :: Double -> Double -> Bool
closeTo d1 d2 = newSig == 0 || newExp < -10
  where (newSig, newExp) = decodeFloat $ abs (d1 - d2)

partitionData :: [[Double]] -> HashMap Double [[Double]]
partitionData newData = fromList $ zip newClasses newTails
  where newClasses = nub $ head <$> newData
        members c  = tail <$> filter (\x -> head x == c) newData
        newTails   = members <$> newClasses

nonMissing :: [Text] -> [Text]
nonMissing fieldValues = filter notMissing fieldValues
  where notMissing fieldValue = unpack fieldValue /= "?"

findMode :: [Text] -> Text
findMode fieldValues = head . head $ filter largest grouped
  where grouped       = group $ sort fieldValues
        maxCount      = maximum $ length <$> grouped
        largest group = length group == maxCount

findMean :: [Double] -> Double
findMean fieldValues = sum fieldValues / fromIntegral (length fieldValues)

