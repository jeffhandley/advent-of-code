module Day7 where

import Debug.Trace (trace)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

data FileEntry pathParts fileSize = FileEntry [String] Int
filePath (FileEntry pathParts fileSize) = pathParts
fileSize (FileEntry pathParts fileSize) = fileSize

data DirectoryListing pathParts fileEntries = DirectoryListing [String] [FileEntry [String] Int]
dirPath (DirectoryListing pathParts fileEntries) = pathParts
files (DirectoryListing pathParts fileEntries) = fileEntries

parentDir :: (DirectoryListing [String] [String]) -> (DirectoryListing [String] [String])
parentDir (DirectoryListing (_:parent) fileEntries) =
  DirectoryListing parent fileEntries

childDir :: (DirectoryListing [String] [String], String) -> (DirectoryListing [String] [String])
childDir (DirectoryListing pathParts fileEntries, childPath) =
  DirectoryListing (childPath:pathParts) fileEntries

rootDir :: (DirectoryListing [String] [String]) -> (DirectoryListing [String] [String])
rootDir (DirectoryListing pathParts fileEntries) =
  DirectoryListing [] fileEntries

-- main :: IO ()
main = do
  args <- getArgs
  inputData <- getLog args
  let pathParts = []
  let fileEntries = []
  let (listing,_) = parseLog (DirectoryListing pathParts fileEntries, lines inputData)

  let first = length (files listing)
  putStrLn (show first)

debug (listing, logLines, message) = trace (show (length logLines) ++ " with " ++ show (length (files listing)) ++ " in " ++ show (dirPath listing) ++ " : " ++ message)

getLog [filename] = readFile filename
getLog ["--", filename] = readFile filename
getLog _ = pure ""

-- parseLog :: (DirectoryListing [String] [String], [String]) ->
--   (DirectoryListing [String] [String], [String])

-- parseCommand :: (DirectoryListing [String] [String], [Char], [String]) ->
--   (DirectoryListing [String] [String], [String])

-- parseDirListing :: (DirectoryListing pathParts fileEntries, [String]) ->
--   (DirectoryListing pathParts fileEntries, [String])

-- parseEntry :: (DirectoryListing pathParts fileEntries, [Char]) ->
--   DirectoryListing pathParts fileEntries

parseLog (listing, ('$':' ':command) : logLines) = do
  let (nextListing, remainingLogLines) = debug (listing, logLines, "[parseLog:]") (parseCommand (listing, command, logLines))
  debug (nextListing, remainingLogLines, "[:parseLog]") parseLog (nextListing, remainingLogLines)

parseLog (listing, []) =
  debug (listing, [], "[parseLog] Done") $
  (listing, [])

parseLog (listing, line : logLines) =
  debug (listing, logLines, "[parseLog] Unrecognized line: " ++ line) $
  parseLog (listing, logLines)

parseCommand (listing, ('c':'d':' ':'.':'.':[]), logLines) =
  debug (listing, logLines, "  [parseCommand] cd ..") $
  (parentDir listing, logLines)

parseCommand (listing, ('c':'d':' ':'/':[]), logLines) =
  debug (listing, logLines, "  [parseCommand] cd /") $
  (rootDir listing, logLines)

parseCommand (listing, ('c':'d':' ':child), logLines) =
  debug (listing, logLines, "  [parseCommand] cd " ++ child) $
  (childDir (listing, child), logLines)

parseCommand (listing, "ls", logLines) = do
  let (newListing, remainingLogLines) = debug (listing, logLines, "  [parseCommand:] ls") (parseDirListing (listing, logLines))
  debug (newListing, remainingLogLines, "  [:parseCommand]") (newListing, remainingLogLines)

parseCommand (listing, cmd, logLines) =
  debug (listing, logLines, "  [parseCommand] Unrecognized command: " ++ cmd) $
  (listing, logLines)

parseDirListing (listing, line:[]) =
  debug (listing, [], "    [parseDirListing] " ++ line) $
  (listing, [])

parseDirListing (listing, line:remainingLogLines) =
  case line of
    ('$':_) -> (listing, (line:remainingLogLines))
    _ -> do
      let newListing = debug (listing, remainingLogLines, "    [parseDirListing] " ++ line) $ parseEntry (listing, line)
      parseDirListing (newListing, remainingLogLines)

parseSize :: [Char] -> Int
parseSize size = read size

parseEntry (listing, ('d':'i':'r':' ':_)) = listing
parseEntry (listing, entry) = case break (== ' ') entry of
  (size, _ : filename) -> do
    let file = FileEntry (filename:(dirPath listing)) (parseSize size)
    DirectoryListing (dirPath listing) (file:(files listing))
  _ -> listing
