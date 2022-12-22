module Day7 where

import Data.Char (isDigit)
import Data.Function
import Data.List
import Data.Map (fromListWith, toList)
import Debug.Trace (trace)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

data FileEntry pathParts fileSize = FileEntry [String] Int
-- filePath (FileEntry pathParts fileSize) = "/" ++ (intercalate "/" (reverse pathParts))
fileSize (FileEntry pathParts fileSize) = fileSize
-- toParent (FileEntry [] fileSize) = FileEntry [] fileSize
-- toParent (FileEntry pathParts fileSize) = FileEntry (tail pathParts) fileSize
-- toFileList fileEntries = map (\(entry) -> (filePath entry, fileSize entry)) fileEntries

data DirectoryListing pathParts fileEntries = DirectoryListing [String] [FileEntry [String] Int]
dirPath (DirectoryListing pathParts fileEntries) = pathParts
files (DirectoryListing pathParts fileEntries) = fileEntries

addFile ((DirectoryListing pathParts fileEntries), filename, size) =
  DirectoryListing pathParts ((FileEntry (filename:pathParts) size):fileEntries)

parentDir :: (DirectoryListing [String] [String]) -> (DirectoryListing [String] [String])
parentDir (DirectoryListing (_:parent) fileEntries) =
  DirectoryListing parent fileEntries

childDir :: (DirectoryListing [String] [String], String) -> (DirectoryListing [String] [String])
childDir (DirectoryListing pathParts fileEntries, childPath) =
  DirectoryListing (childPath:pathParts) fileEntries

rootDir :: (DirectoryListing [String] [String]) -> (DirectoryListing [String] [String])
rootDir (DirectoryListing pathParts fileEntries) =
  DirectoryListing [] fileEntries

main = do
  args <- getArgs
  inputData <- getLog args
  let listing = parseLog (DirectoryListing [] [], lines inputData)



  putStrLn (show (length (files listing)))

-- aggregateSizes fileList | length fileList == 1 = fileList

-- aggregateSizes fileList = do
--   let parents = toFileList (map toParent fileList)
--   let grouped = fromListWith (+) asList

--   length aggregated

getLog [filename] = readFile filename
getLog ["--", filename] = readFile filename
getLog _ = pure ""

parseLog :: (DirectoryListing [String] [String], [String]) -> DirectoryListing [String] [String]
parseLog (listing, []) = listing
parseLog (listing, ('$':' ':command) : logLines) = parseLog (parseCommand (listing, command, logLines))
parseLog (listing, line : logLines) = parseLog (listing, logLines)

parseCommand :: (DirectoryListing [String] [String], [Char], [String]) ->
  (DirectoryListing [String] [String], [String])

parseCommand (listing, ('c':'d':' ':'.':'.':[]), logLines) = (parentDir listing, logLines)
parseCommand (listing, ('c':'d':' ':'/':[]), logLines) = (rootDir listing, logLines)
parseCommand (listing, ('c':'d':' ':child), logLines) = (childDir (listing, child), logLines)
parseCommand (listing, "ls", logLines) = parseDirListing (listing, logLines)
parseCommand (listing, cmd, logLines) = (listing, logLines)

parseDirListing :: (DirectoryListing pathParts fileEntries, [String]) ->
  (DirectoryListing pathParts fileEntries, [String])

parseDirListing (listing, []) = (listing, [])
parseDirListing (listing, logLines) | head (head logLines) == '$' = (listing, logLines)
parseDirListing (listing, logLines) = parseDirListing (parseEntry (listing, words (head logLines)), tail logLines)

parseEntry :: (DirectoryListing pathParts fileEntries, [String]) ->
  DirectoryListing pathParts fileEntries

parseEntry (listing, (size:filename:[])) | isDigit(head size) = addFile (listing, filename, read size)
parseEntry (listing, (size:filename:[])) | size == "dir" = listing
