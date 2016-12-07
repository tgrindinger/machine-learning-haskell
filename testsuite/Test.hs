import Test.HUnit
import Test.QuickCheck
import InputProcessorTest
import NaiveBayesTest

main :: IO ()
main = do
  testInputProcessor
  -- testFieldProcessor
  -- testCatFieldProcessor
  -- testClassFieldProcessor
  -- testDistFieldProcessor
  -- testUtils
  testNaiveBayes
  return ()

