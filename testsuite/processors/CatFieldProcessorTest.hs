module CatFieldProcessorTest where

import Test.HUnit
import Data.Text (pack)
import Data.HashMap.Strict (HashMap, fromList)
import CatFieldProcessor

testCatFieldProcessor = do
  putStrLn "Testing CatFieldProcessor..."
  runTestTT catFieldProcessorTests

catFieldProcessorTests = TestList [TestLabel "test construct"   testConstruct,
                                   TestLabel "test processData" testProcessData]

testList = pack <$> ["a", "b", "c", "b", "?"]

testConstruct = TestCase (do
  let newCatMap = fromList [(pack "a", 0), (pack "b", 1), (pack "c", 2), (pack "?", 1)]
  assertEqual "construct" newCatMap . catMap $ construct testList
  )

testProcessData = TestCase (do
  let catProc = construct testList
  assertEqual "processData" [1, 0, 0] . processData catProc $ pack "a"
  assertEqual "processData" [0, 1, 0] . processData catProc $ pack "b"
  assertEqual "processData" [0, 0, 1] . processData catProc $ pack "c"
  assertEqual "processData" [0, 1, 0] . processData catProc $ pack "?"
  )

