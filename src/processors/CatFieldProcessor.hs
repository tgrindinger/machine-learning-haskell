module CatFieldProcessor where

import Utils
import Data.List hiding (insert)
import Data.Text (Text, pack, unpack)
import Data.HashMap.Strict (HashMap, (!), fromList, insert, keys)

data CatFieldProcessor = CatFieldProcessor { catMap :: HashMap Text Int }

construct :: [Text] -> CatFieldProcessor
construct newData = CatFieldProcessor addMissing
  where newCatMap  = fromList $ zip (nub newData) [0..]
        newMode    = findMode $ nonMissing newData
        addMissing = insert (pack "?") (newCatMap ! newMode) newCatMap

processData :: CatFieldProcessor -> Text -> [Double]
processData processor newValue = newList
  where list = replicate (length (keys $ catMap processor) - 1) 1 :: [Double]
        index = catMap processor ! newValue
        newList = take index list ++ [2] ++ drop (index + 1) list

