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

parseLog [] = pure ()
parseLog (('$':' ':command):logLines) = parseCommand (command, logLines)
parseLog logLine = pure ()

parseCommand (('c':'d':' ':cd), logLines) = do
  putStrLn ("CD: " ++ cd)
  parseLog logLines

parseCommand ("ls", logLines) = do
  let (size, dirs, remaining) = parseDirListing (0, [], logLines)
  putStrLn ("COMMAND: ls --> Files: " ++ (show size) ++ " | Dirs: " ++ (show (length dirs)))
  parseLog remaining

parseCommand (cmd, logLines) = do
  putStrLn ("UNKNOWN COMMAND: " ++ cmd)
  parseLog logLines

parseDirListing :: (Int, [String], [String]) -> (Int, [String], [String])
parseDirListing (dirSize, dirs, line : logLines) = do
  let (newDirs, fileSize) = parseEntry (dirs, line)
  let next = head logLines
  let result = (dirSize + fileSize, newDirs, logLines)

  case head logLines of
    ('$':' ':_) -> result
    _ -> parseDirListing result

parseEntry :: ([String], [Char]) -> ([String], Int)
parseEntry (dirs, ('d':'i':'r':' ':dir)) = (dir : dirs, 0)
parseEntry (dirs, entry) = case break (== ' ') entry of
  (size, _ : filename) -> (dirs, read size)
  _ -> (dirs, 0)
