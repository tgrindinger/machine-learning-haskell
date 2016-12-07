module FieldProcessor where

import Data.Text
import qualified CatFieldProcessor as CatP
import qualified ClassFieldProcessor as ClassP
import qualified DistFieldProcessor as DistP

data FieldProcessor
  = CatInstance   CatP.CatFieldProcessor 
  | ClassInstance ClassP.ClassFieldProcessor 
  | DistInstance  DistP.DistFieldProcessor

processData :: FieldProcessor -> Text -> [Double]
processData (CatInstance   p) d = CatP.processData   p d
processData (ClassInstance p) d = ClassP.processData p d
processData (DistInstance  p) d = DistP.processData  p d

construct :: Text -> [Text] -> FieldProcessor
construct procType list
  | procType == pack "Category"     = CatInstance   (CatP.construct   list)
  | procType == pack "Class"        = ClassInstance (ClassP.construct list)
  | procType == pack "Distribution" = DistInstance  (DistP.construct  list)

showType :: FieldProcessor -> Text
showType (CatInstance   p) = pack "Category"
showType (ClassInstance p) = pack "Class"
showType (DistInstance  p) = pack "Distribution"

