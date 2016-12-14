module InputProcessorTest where

import Test.HUnit
import InputProcessor
import FieldProcessor (FieldProcessor(CatInstance, ClassInstance, DistInstance))
import Data.Text (pack)
import Data.HashMap.Strict (HashMap, fromList)
import qualified CatFieldProcessor as CatP
import qualified ClassFieldProcessor as ClassP
import qualified DistFieldProcessor as DistP

testInputProcessor = do
  putStrLn "Testing InputProcessor..."
  runTestTT inputProcessorTests

-- TODO:
-- construct :: [[Text]] -> Maybe Int -> InputProcessor

inputProcessorTests = TestList [TestLabel "test actualClass"   testActualClass,
                                TestLabel "test adjustTypes"   testAdjustTypes,
                                TestLabel "test numCapable"    testNumCapable,
                                TestLabel "test suggestType"   testSuggestType,
                                TestLabel "test attrRange"     testAttrRange,
                                TestLabel "test processRecord" testProcessRecord]

testActualClass = TestCase (do
  assertEqual "with nothing" 5 (actualClass Nothing 5)
  assertEqual "with just"    4 (actualClass (Just 4) 5)
  )

testAdjustTypes = TestCase (do
  let oldTypes = pack <$> ["Category", "Category", "Distribution"]
      newTypes = pack <$> ["Category", "Class",    "Distribution"]
  assertEqual "with typical" newTypes $ adjustTypes oldTypes 1
  )

-- TODO:
-- processData :: InputProcessor -> [[Text]] -> [[Double]]

defaultRecordData = (classMap, catMap, distMean)
  where classMap = fromList [(pack "a", 0), (pack "b", 1), (pack "?", 1)]
        catMap   = fromList [(pack "c", 0), (pack "d", 1), (pack "?", 0)]
        distMean = 2

makeFullRecord = (resultFull, procFull, recordFull)
  where recordFull        = pack <$> ["a", "1", "c", "2"]
        (classMap, catMap, distMean) = defaultRecordData
        fieldProcsFull    = [ClassInstance $ ClassP.ClassFieldProcessor classMap,
                             DistInstance  $ DistP.DistFieldProcessor distMean,
                             CatInstance   $ CatP.CatFieldProcessor catMap,
                             DistInstance  $ DistP.DistFieldProcessor distMean]
        classIndexFull    = 0
        procFull          = InputProcessor fieldProcsFull classIndexFull
        resultFull        = [0, 1, 2, 1, 2]

makeMissingRecord = (resultMissing, procMissing, recordMissing)
  where recordMissing     = pack <$> ["d", "?", "?", "?"]
        (classMap, catMap, distMean) = defaultRecordData
        fieldProcsMissing = [CatInstance   $ CatP.CatFieldProcessor catMap,
                             DistInstance  $ DistP.DistFieldProcessor distMean,
                             ClassInstance $ ClassP.ClassFieldProcessor classMap,
                             CatInstance   $ CatP.CatFieldProcessor catMap]
        classIndexMissing = 2
        procMissing       = InputProcessor fieldProcsMissing classIndexMissing
        resultMissing     = [1, 1, 2, 2, 2, 1]

testProcessRecord = TestCase (do
  let (resultFull,    procFull,    recordFull)    = makeFullRecord
      (resultMissing, procMissing, recordMissing) = makeMissingRecord
  assertEqual "with full"    resultFull    $ processRecord procFull    recordFull
  assertEqual "with missing" resultMissing $ processRecord procMissing recordMissing
  )

-- TODO:
-- processSample :: InputProcessor -> [Text] -> [Double]

-- TODO:
-- evaluateData :: InputProcessor -> [[Text]] -> Text

-- TODO:
-- suggestTypes :: [[Text]] -> [Text]

testSuggestType = TestCase (do
  let catList  = pack <$> ["a", "b", "?", "d"]
      distList = pack <$> ["1", "2", "?", "4"]
  assertEqual "with category"     (pack "Category")     $ suggestType catList
  assertEqual "with distribution" (pack "Distribution") $ suggestType distList
  )

-- TODO:
-- suggestClass :: [[Text]] -> Int

testAttrRange = TestCase (do
  let catAttrs = pack <$> ["az", "b", "?", "d"]
      distAttrs = pack <$> ["1", "2", "?", "14"]
  assertEqual "with category"     (pack "az", pack "d")     $ attrRange catAttrs
  assertEqual "with distribution" (pack "1.0", pack "14.0") $ attrRange distAttrs
  )

testNumCapable = TestCase (do
  assertEqual "with category"     False $ numCapable $ pack <$> ["a", "b", "?", "d"]
  assertEqual "with distribution" True  $ numCapable $ pack <$> ["1", "2", "?", "4"]
  )

