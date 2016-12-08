module FieldProcessorTest where

import Test.HUnit
import FieldProcessor (FieldProcessor(CatInstance, ClassInstance, DistInstance), showType)
import Data.Text (pack)
import Data.HashMap.Strict (HashMap, fromList)
import qualified CatFieldProcessor as CatP
import qualified ClassFieldProcessor as ClassP
import qualified DistFieldProcessor as DistP

testFieldProcessor = do
  putStrLn "Testing FieldProcessor..."
  runTestTT fieldProcessorTests

fieldProcessorTests = TestList [TestLabel "test showType" testShowType]

testShowType = TestCase (do
  let classMap  = fromList [(pack "?", 0)]
      catMap    = fromList [(pack "?", 0)]
      distMean  = 2
      catProc   = CatInstance   $ CatP.CatFieldProcessor     catMap
      classProc = ClassInstance $ ClassP.ClassFieldProcessor classMap
      distProc  = DistInstance  $ DistP.DistFieldProcessor   distMean
  assertEqual "with category"     (pack "Category")     (showType catProc)
  assertEqual "with class"        (pack "Class")        (showType classProc)
  assertEqual "with distribution" (pack "Distribution") (showType distProc)
  )

