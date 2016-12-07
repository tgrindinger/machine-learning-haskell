module DistFieldProcessor where

import Utils
import Data.Text

data DistFieldProcessor = DistFieldProcessor { mean :: Double }

construct :: [Text] -> DistFieldProcessor
construct list = DistFieldProcessor newMean
  where newMean = findMean $ read . unpack <$> nonMissing list

processData :: DistFieldProcessor -> Text -> [Double]
processData processor d = [if d == pack "?" then mean processor else convertToDouble d]

