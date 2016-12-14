import Test.HUnit
import Test.QuickCheck
import InputProcessorTest
import FieldProcessorTest
import CatFieldProcessorTest
import ClassFieldProcessorTest
import DistFieldProcessorTest
import NaiveBayesTest
import CrossValidatorTest
import UtilsTest

main :: IO ()
main = do
  testInputProcessor
  testFieldProcessor
  testCatFieldProcessor
  testClassFieldProcessor
  testDistFieldProcessor
  testCrossValidator
  testUtils
  testNaiveBayes
  return ()

