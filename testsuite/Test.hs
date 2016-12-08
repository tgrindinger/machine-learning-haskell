import Test.HUnit
import Test.QuickCheck
import InputProcessorTest
import FieldProcessorTest
import CatFieldProcessorTest
import ClassFieldProcessorTest
import DistFieldProcessorTest
import NaiveBayesTest

main :: IO ()
main = do
  testInputProcessor
  testFieldProcessor
  testCatFieldProcessor
  testClassFieldProcessor
  testDistFieldProcessor
  -- testUtils
  testNaiveBayes
  return ()

