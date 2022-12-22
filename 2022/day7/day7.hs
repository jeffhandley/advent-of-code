module Day7 where

import Data.Char (isDigit)
import Data.Function
import Data.List
import Data.Maybe (fromMaybe)
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

formatEntry :: (([String], Int) -> Maybe String) -> ([String], Int) -> String
formatEntry indicator = (\(path, size) -> formatPath path ++ " : " ++ show size ++ fromMaybe "" (indicator (path, size)))

formatEntries :: (([String], Int) -> Maybe String) -> [([String], Int)] -> String
formatEntries indicator = (\entries -> intercalate "\n" (map (formatEntry indicator) entries))

sortByPath :: [([String], size)] -> [([String], size)]
sortByPath = sortBy (compare `on` (\(path, _) -> formatPath path))

main = do
  commandLog <- readFile "input.log"

  -- Process the log to build the list of directories with their total sizes
  -- Flatten the ancestors to produce a full (flattened) tree of directories and sizes
  let dirEntries = flattenAncestors (parentEntries (processLog commandLog))

  -- Part One: Find directories with accumulated sizes under the size limit.
  --           Sum up those directories' accumulated sizes (without de-duping the sizes) for the answer.
  --           Sort the directories by path for sake of display
  let sizeLimit = 100000
  let withinLimit = (\(_, size) -> size <= sizeLimit)
  let indicateWithinLimit = (\entry -> if withinLimit entry then Just " ** SMALL ENOUGH **" else Nothing)

  let (sumWithinLimit, dirsWithinLimit) = accumulateSizes (filter withinLimit dirEntries)

  putStrLn ("~~~~~~~~~~~~~~~~~~ PART ONE ~~~~~~~~~~~~~~~~~~")
  putStrLn (formatEntries indicateWithinLimit (sortByPath dirEntries))
  putStrLn ("")
  putStrLn ("Number of directories within limit:     " ++ show (length dirsWithinLimit))
  putStrLn ("Total size of directories within limit: " ++ show sumWithinLimit ++ " [PART ONE ANSWER]")

  -- Part Two: Find directories with accumulated sizes above the space needed to free up.
  --           Then identify the smallest directory that can be deleted to free up enough space.
  --           To prepare, we first need to calculate how much disk space is used and how
  --           much space needs to be freed up. We do that by finding the root directory's size.
  let diskCapacity = 70000000
  let spaceNeeded  = 30000000

  let totalSizeUsed = maybe 0 snd (find (\(path, size) -> length path == 0) dirEntries)
  let (freeSpace, spaceToFree) = (diskCapacity - totalSizeUsed, spaceNeeded - freeSpace)
  let bigEnough = (\(_, size) -> size >= spaceToFree)
  let indicateBigEnough = (\entry -> if bigEnough entry then Just " ** BIG ENOUGH **" else Nothing)

  let (dirToDelete, spaceFreed) = findSmallest (filter bigEnough dirEntries)

  putStrLn ("")
  putStrLn ("~~~~~~~~~~~~~~~~~~ PART TWO ~~~~~~~~~~~~~~~~~~")
  putStrLn (formatEntries indicateBigEnough (sortByPath dirEntries))
  putStrLn ("")
  putStrLn ("Total Disk Capacity : " ++ show diskCapacity)
  putStrLn ("Space Needed        : " ++ show spaceNeeded)
  putStrLn ("Total Space Used    : " ++ show totalSizeUsed)
  putStrLn ("Total Space Free    : " ++ show freeSpace)
  putStrLn ("Space to Free       : " ++ show spaceToFree)
  putStrLn ("")
  putStrLn ("Directory to delete : " ++ formatPath dirToDelete)
  putStrLn ("Space to be Freed   : " ++ show spaceFreed ++ " [PART TWO ANSWER]")


findSmallest :: [(path, Int)] -> (path, Int)
findSmallest = minimumBy (compare `on` snd)

accumulateSizes :: [(path, Int)] -> (Int, [(path, Int)])
accumulateSizes = mapAccumL (\acc (path, size) -> (acc + size, (path, size))) 0

isDescendent :: ([String], Int) -> Bool
isDescendent entry = length (fst entry) > 0

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
parseCommand (listing, ('c':'d':' ':child), logLines) | child == ".." = (parentDir listing, logLines)
parseCommand (listing, ('c':'d':' ':child), logLines) | child == "/" = (rootDir listing, logLines)
parseCommand (listing, ('c':'d':' ':child), logLines) = (childDir (listing, child), logLines)
parseCommand (listing, "ls", logLines) = parseDirListing (listing, logLines)
parseCommand (listing, cmd, logLines) = (listing, logLines)

parseDirListing :: (DirectoryListing pathParts fileEntries, [String]) -> (DirectoryListing pathParts fileEntries, [String])
parseDirListing (listing, []) = (listing, [])
parseDirListing (listing, logLines) | head (head logLines) == '$' = (listing, logLines)
parseDirListing (listing, logLines) = parseDirListing (parseEntry (listing, words (head logLines)), tail logLines)

parseEntry :: (DirectoryListing pathParts fileEntries, [String]) -> DirectoryListing pathParts fileEntries
parseEntry (listing, (sizeOrDir:filename:[])) | sizeOrDir == "dir" = listing
parseEntry (listing, (sizeOrDir:filename:[])) = addFile (listing, filename, read sizeOrDir)
