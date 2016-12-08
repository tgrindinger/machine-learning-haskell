module ClassFieldProcessorTest where

import Test.HUnit
import Data.Text (pack)
import Data.HashMap.Strict (HashMap, fromList)
import ClassFieldProcessor

testClassFieldProcessor = do
  putStrLn "Testing ClassFieldProcessor..."
  runTestTT classFieldProcessorTests

classFieldProcessorTests = TestList [TestLabel "test construct"   testConstruct,
                                     TestLabel "test processData" testProcessData]

testList = pack <$> ["a", "b", "c", "b", "?"]

testConstruct = TestCase (do
  let newCatMap = fromList [(pack "a", 0), (pack "b", 1), (pack "c", 2), (pack "?", 1)]
      list      = pack <$> ["a", "b", "b", "c", "?"]
  assertEqual "construct" newCatMap . classMap $ construct list
  )

testProcessData = TestCase (do
  let classProc = construct testList
  assertEqual "processData" [0] . processData classProc $ pack "a"
  assertEqual "processData" [1] . processData classProc $ pack "b"
  assertEqual "processData" [2] . processData classProc $ pack "c"
  assertEqual "processData" [1] . processData classProc $ pack "?"
  )

