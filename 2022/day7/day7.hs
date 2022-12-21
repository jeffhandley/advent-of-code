module Day7 where

import Data.Char (isDigit)
import Data.Function
import Data.List
import Data.Map (fromList, fromListWith, toList)
import Debug.Trace (trace)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

data DirectoryListing pathParts fileEntries = DirectoryListing [String] [([String], Int)]
dirPath (DirectoryListing pathParts fileEntries) = pathParts
files (DirectoryListing pathParts fileEntries) = fileEntries
addFile ((DirectoryListing pathParts fileEntries), filename, size) =
  DirectoryListing pathParts ((filename:pathParts, size):fileEntries)

parentDir :: (DirectoryListing [String] [String]) -> (DirectoryListing [String] [String])
parentDir (DirectoryListing (_:parent) fileEntries) = DirectoryListing parent fileEntries

childDir :: (DirectoryListing [String] [String], String) -> (DirectoryListing [String] [String])
childDir (DirectoryListing parent fileEntries, path) = DirectoryListing (path:parent) fileEntries

rootDir :: (DirectoryListing [String] [String]) -> (DirectoryListing [String] [String])
rootDir (DirectoryListing pathParts fileEntries) = DirectoryListing [] fileEntries

formatPath :: [String] -> String
formatPath pathParts = '/':(intercalate "/" (reverse pathParts))

formatEntries :: [(String, Int)] -> String
formatEntries entries = intercalate "\n" (map (\(path, size) -> path ++ " : " ++ show size) entries)

sizeLimit = 100000

main = do
  commandLog <- readFile "input/command.log"

  let (dirEntries, sumWithinLimit) = sumSizesWithinLimit (withinLimit sizeLimit (flattenAncestors (parentEntries (processLog commandLog))))

  putStrLn ("\n\n" ++ (formatEntries dirEntries))
  putStrLn ("\nFolders within limit: " ++ show (length dirEntries) ++ ". Total size: " ++ show sumWithinLimit)


-- ancestorsOnly :: [([String], Int)] -> [([String], Int)]
-- ancestorsOnly dirEntries = filter (\(path:parentPath, _) -> isParentmost (parentPath, dirEntries)) dirEntries

sumSizesWithinLimit :: [([String], Int)] -> ([(String, Int)], Int)
sumSizesWithinLimit entries = (map (\(path, size) -> (formatPath path, size)) entries, sumSizes entries)

withinLimit :: Int -> [([String], Int)] -> [([String], Int)]
withinLimit sizeLimit = filter (\(path, size) -> size <= sizeLimit)

sumSizes :: [(path, Int)] -> Int
sumSizes [] = 0
sumSizes ((_, size):entries) = size + sumSizes entries

isDescendent :: ([String], Int) -> Bool
isDescendent (path, _) = length path > 1

isParentmost :: ([String], [([String], Int)]) -> Bool
isParentmost (parentPath, list) = (find (\(path, _) -> path == parentPath) list) == Nothing

entriesWithAncestors :: [([String], Int)] -> [([String], Int)]
entriesWithAncestors = filter isDescendent

parentEntry :: ([String], Int) -> ([String], Int)
parentEntry (path:parent, size) = (parent, size)

parentEntries :: [([String], Int)] -> [([String], Int)]
parentEntries fileEntries = map parentEntry (entriesWithAncestors fileEntries)

flattenAncestors :: [([String], Int)] -> [([String], Int)]
flattenAncestors [] = []
flattenAncestors fileList = toList(fromListWith (+) (fileList ++ flattenAncestors (parentEntries fileList)))

processLog :: String -> [([String], Int)]
processLog commandLog = parseCommandLines (DirectoryListing [] [], lines commandLog)

parseCommandLines :: (DirectoryListing [String] [String], [String]) -> [([String], Int)]
parseCommandLines (listing, []) = files listing
parseCommandLines (listing, ('$':' ':command) : logLines) = parseCommandLines (parseCommand (listing, command, logLines))

parseCommand :: (DirectoryListing [String] [String], [Char], [String]) -> (DirectoryListing [String] [String], [String])
parseCommand (listing, ('c':'d':' ':'.':'.':[]), logLines) = (parentDir listing, logLines)
parseCommand (listing, ('c':'d':' ':'/':[]), logLines) = (rootDir listing, logLines)
parseCommand (listing, ('c':'d':' ':child), logLines) = (childDir (listing, child), logLines)
parseCommand (listing, "ls", logLines) = parseDirListing (listing, logLines)
parseCommand (listing, cmd, logLines) = (listing, logLines)

parseDirListing :: (DirectoryListing pathParts fileEntries, [String]) -> (DirectoryListing pathParts fileEntries, [String])
parseDirListing (listing, []) = (listing, [])
parseDirListing (listing, logLines) | head (head logLines) == '$' = (listing, logLines)
parseDirListing (listing, logLines) = parseDirListing (parseEntry (listing, words (head logLines)), tail logLines)

parseEntry :: (DirectoryListing pathParts fileEntries, [String]) -> DirectoryListing pathParts fileEntries
parseEntry (listing, (size:filename:[])) | isDigit(head size) = addFile (listing, filename, read size)
parseEntry (listing, (size:filename:[])) | size == "dir" = listing
