module Day7 where

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Numeric.Natural

main :: IO ()
main = do
  args <- getArgs
  let file = getInputFilename args
  inputData <- getInputData file
  putStrLn inputData

getInputFilename :: [String] -> String
getInputFilename args =
  case args of
    [filename] -> filename
    ["--", filename] -> filename
    _ -> ""

getInputData "" = pure ""
getInputData filename = readFile filename
