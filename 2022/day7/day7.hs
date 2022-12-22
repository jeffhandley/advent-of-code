module Day7 where

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Numeric.Natural

main :: IO ()
main = do
  args <- getArgs
  inputData <- getLog args
  parseLog (lines inputData)

getLog [filename] = readFile filename
getLog ["--", filename] = readFile filename
getLog _ = pure ""

parseLog logLines = do
  case logLines of
    (line:remaining) -> do
      parseLogLine line
      parseLog remaining
    [] -> pure ()

parseLogLine "" = pure ()
parseLogLine line = do
  putStrLn line
