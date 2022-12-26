-- https://adventofcode.com/2022/day/7

module Day7 where

import Data.Function (on)
import Data.List (find, intercalate, mapAccumL, minimumBy, sortBy)
import Data.Map (fromListWith, toList)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

main = do
  -- https://adventofcode.com/2022/day/7/input
  commandLog <- readFile "input.log"

  -- Process the log to build the list of directories with their total sizes
  -- Flatten the ancestors to produce a full (flattened) tree of directories and sizes
  let dirEntries = flattenAncestors (parentEntries (processLog commandLog))

  {- Part One: Find directories with accumulated sizes under the size limit.
               Sum up those directories' accumulated sizes (without de-duping the sizes) for the answer.
               Sort the directories by path for sake of display. -}
  let sizeLimit = 100000
  let withinLimit = (\(_, size) -> size <= sizeLimit)
  let indicateWithinLimit = (\entry -> if withinLimit entry then Just " ** SMALL ENOUGH **" else Nothing)

  let (sumWithinLimit, dirsWithinLimit) = accumulateSizes (filter withinLimit dirEntries)

  putStrLn ("~~~~~~~~~~~~~~~~~~ PART ONE ~~~~~~~~~~~~~~~~~~")
  putStrLn (formatEntries indicateWithinLimit (sortByPath dirEntries))
  putStrLn ("")
  putStrLn ("Number of directories within limit:     " ++ show (length dirsWithinLimit))
  putStrLn ("Total size of directories within limit: " ++ show sumWithinLimit ++ " [PART ONE ANSWER]")

  {- Part Two: Find directories with accumulated sizes above the space needed to free up.
               Then identify the smallest directory that can be deleted to free up enough space.
               To prepare, we first need to calculate how much disk space is used and how
               much space needs to be freed up. We do that by finding the root directory's size. -}
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


{- Process the command log to extract the directory listing of the file system
   revealed by the log. The log contains commands (`cd /`, `cd ..`, `cd {dir}`, and `ls`).
   The output of the `ls` command includes `dir {dir}` lines (which we can ignore) and
   `{size} {file}` lines that provide file/size data for the current directory. That data
   is used to construct full list of files that exist across the file system. -}
processLog :: String -> [([String], Int)]
processLog commandLog = parseCommandLog (DirectoryListing [] [], lines commandLog)

parseCommandLog :: (DirectoryListing [String] [String], [String]) -> [([String], Int)]
parseCommandLog (listing, []) = files listing
parseCommandLog (listing, ('$':' ':'c':'d':' ':dir):logLines) = parseCommandLog ((changeDirectory dir listing), logLines)
parseCommandLog (listing, ("$ ls"):logLines) = parseCommandLog (parseDirListing listing logLines)

parseDirListing :: DirectoryListing pathParts fileEntries -> [String] -> (DirectoryListing pathParts fileEntries, [String])
parseDirListing listing [] = (listing, [])
parseDirListing listing logLines | head (head logLines) == '$' = (listing, logLines)
parseDirListing listing logLines = parseDirListing (parseEntry (listing, words (head logLines))) (tail logLines)

parseEntry :: (DirectoryListing pathParts fileEntries, [String]) -> DirectoryListing pathParts fileEntries
parseEntry (listing, (sizeOrDir:filename:[])) | sizeOrDir == "dir" = listing
parseEntry (listing, (sizeOrDir:filename:[])) = addFile (listing, filename, read sizeOrDir)

{- State management of the DirectoryListing, including the current directory, and all files
   * rootDir, parentDir, and childDir for changing the current directory
   * addFile for adding a file entry at the current directory
-}
data DirectoryListing pathParts fileEntries = DirectoryListing [String] [([String], Int)]
dirPath (DirectoryListing pathParts fileEntries) = pathParts
files (DirectoryListing pathParts fileEntries) = fileEntries
addFile ((DirectoryListing pathParts fileEntries), filename, size) =
  DirectoryListing pathParts ((filename:pathParts, size):fileEntries)

changeDirectory :: String -> DirectoryListing [String] [String] -> DirectoryListing [String] [String]
changeDirectory "/" = rootDir
changeDirectory ".." = parentDir
changeDirectory dir = childDir dir

rootDir :: DirectoryListing [String] [String] -> DirectoryListing [String] [String]
rootDir listing = DirectoryListing [] (files listing)

parentDir :: DirectoryListing [String] [String] -> DirectoryListing [String] [String]
parentDir (DirectoryListing (_:parent) fileEntries) = DirectoryListing parent fileEntries

childDir :: String -> DirectoryListing [String] [String] -> DirectoryListing [String] [String]
childDir dir listing = DirectoryListing (dir:(dirPath listing)) (files listing)

{- Logic for processing the file entries to produce entries
   for all ancestor directories, accumulating the total size
   of all file entries contained within each directory. -}
entriesWithAncestors :: [([String], Int)] -> [([String], Int)]
entriesWithAncestors = filter isDescendent

isDescendent :: ([String], Int) -> Bool
isDescendent entry = length (fst entry) > 0

parentEntry :: ([String], Int) -> ([String], Int)
parentEntry (path:parent, size) = (parent, size)

parentEntries :: [([String], Int)] -> [([String], Int)]
parentEntries fileEntries = map parentEntry (entriesWithAncestors fileEntries)

flattenAncestors :: [([String], Int)] -> [([String], Int)]
flattenAncestors [] = []
flattenAncestors fileList = toList(fromListWith (+) (fileList ++ flattenAncestors (parentEntries fileList)))

{- Business logic for processing the flattened list of entries. -}

-- Given a list of entries, calculate the total size of those entries.
-- Return both the total size and the entries.
accumulateSizes :: [(path, Int)] -> (Int, [(path, Int)])
accumulateSizes = mapAccumL (\acc (path, size) -> (acc + size, (path, size))) 0

-- Given a list of entries, find the entry with the smallest total size.
findSmallest :: [(path, Int)] -> (path, Int)
findSmallest = minimumBy (compare `on` snd)

{- Functions for printing entries to the console in readable formats. -}

-- Format path parts into a path string
formatPath :: [String] -> String
formatPath pathParts = '/':(intercalate "/" (reverse pathParts))

-- Format an entry into its path string, size, and an optional indicator suffix
formatEntry :: (([String], Int) -> Maybe String) -> ([String], Int) -> String
formatEntry indicator = (\(path, size) -> formatPath path ++ " : " ++ show size ++ fromMaybe "" (indicator (path, size)))

-- Format a list of entries into a single string with one entry per line
formatEntries :: (([String], Int) -> Maybe String) -> [([String], Int)] -> String
formatEntries indicator = (\entries -> intercalate "\n" (map (formatEntry indicator) entries))

-- Sort a list of entries by the formatted path, which groups sub-directories under ancestors
sortByPath :: [([String], size)] -> [([String], size)]
sortByPath = sortBy (compare `on` (\(path, _) -> formatPath path))
