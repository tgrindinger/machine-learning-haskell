module ClassFieldProcessor where

import Utils
import Data.List hiding (insert)
import Data.Text (Text, pack)
import Data.HashMap.Strict (HashMap, (!), fromList, insert)

data ClassFieldProcessor = ClassFieldProcessor { classMap :: HashMap Text Double }

construct :: [Text] -> ClassFieldProcessor
construct newData = ClassFieldProcessor addMissing
  where newClassMap = fromList $ zip (nub newData) [0..]
        newMode     = findMode $ nonMissing newData
        addMissing  = insert (pack "?") (newClassMap ! newMode) newClassMap

processData :: ClassFieldProcessor -> Text -> [Double]
processData processor d = [classMap processor ! d]

