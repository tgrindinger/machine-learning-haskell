module DistFieldProcessorTest where

import Test.HUnit
import Data.Text (pack)
import Data.HashMap.Strict (HashMap, fromList)
import DistFieldProcessor

testDistFieldProcessor = do
  putStrLn "Testing DistFieldProcessor..."
  runTestTT distFieldProcessorTests

distFieldProcessorTests = TestList [TestLabel "test construct"   testConstruct,
                                    TestLabel "test processData" testProcessData]

testList = pack <$> ["0", "1", "3", "4", "?"]

testConstruct = TestCase (do
  assertEqual "construct" 2 . mean $ construct testList
  )

testProcessData = TestCase (do
  let distProc = construct testList
  assertEqual "processData" [0] . processData distProc $ pack "0"
  assertEqual "processData" [2] . processData distProc $ pack "2"
  assertEqual "processData" [5] . processData distProc $ pack "5"
  assertEqual "processData" [2] . processData distProc $ pack "?"
  )

