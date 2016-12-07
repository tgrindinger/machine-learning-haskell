import Prelude hiding (readFile, words, lines)
import System.IO hiding (readFile)
import Data.List hiding (words, lines)
import Data.Text hiding (intercalate)
import Data.Text.IO (readFile)
import CrossValidator
import Control.Exception
import NaiveBayes (means, variances)
import qualified NaiveBayes as NB
import InputProcessor (processData, processSample, evaluateData)
import qualified InputProcessor as IP

main :: IO ()
main = do
  filename   <- prompt "Enter name of data file: "
  classIndex <- prompt "Enter index of class (blank for automatic): "
  numFolds   <- read <$> prompt "Number of folds: "
  newData    <- (fmap words . lines) <$> readFile filename
  let newClassIndex   = if classIndex == "" then Nothing else Just $ read classIndex
      processor       = IP.construct newData newClassIndex
      processedData   = processData processor newData
      classifier      = NB.construct processedData
      validation      = crossValidate numFolds processedData
      output          = ["",
                         "means: " ++ show (means classifier),
                         "variances: " ++ show (variances classifier),
                         "",
                         "cross validation: " ++ show validation,
                         ""]
  catch (putStrLn $ unpack (evaluateData processor newData) ++ "\n" ++
           intercalate "\n" output)
        $ \(SomeException err) -> print err

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine
  
