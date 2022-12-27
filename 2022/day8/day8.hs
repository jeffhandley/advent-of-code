-- https://adventofcode.com/2022/day/8

module Day8 where

import Control.Monad (liftM2)
import Data.Char (digitToInt)
import Data.List (maximumBy, partition, transpose)

main = do
  -- https://adventofcode.com/2022/day/8/input
  inputData <- readFile "input.txt"

  -- Each line represents a row of trees; each col is the tree height
  -- Map this into a grid of trees with coordinates and heights
  let forest = mapForest (map (map digitToInt) (lines inputData))

  {- PART ONE: Count the trees that are visible from any edge of the forest -}
  let visibleTrees = filter (\(((x, y), z), visible) -> visible) (mapVisibility forest)

  putStrLn (unlines (map formatTreeVisibility visibleTrees))
  putStrLn ""
  putStrLn ("Trees Visible: " ++ show (length visibleTrees) ++ " [PART ONE ANSWER]")

  {- PART TWO: Calculate the Scenic Score for every tree and find the highest score
               The Scenic Score is the product of how many trees are visible in all
               directions. Visibility is based on line of sight between a tree and
               an equal or taller tree, with that tree plus all trees between. -}

-- Gets the length of the forest (Y axis)
forestLength :: [[Int]] -> Int
forestLength rows = length rows

-- Gets the width of the forest (X axis), assuming all rows are the same width
forestWidth :: [[Int]] -> Int
forestWidth rows = length (rows !! 0)

-- Maps tree rows into a single list representing the tree grid
mapForest :: [[Int]] -> [((Int, Int), Int)]
mapForest treeRows = liftM2 (\row col -> getTree col row treeRows) [0..(forestLength treeRows) - 1] [0..(forestWidth treeRows) - 1]

-- Creates a tree representation with its grid coordinates and height
getTree :: Int -> Int -> [[Int]] -> ((Int, Int), Int)
getTree col row trees = ((col, row), trees !! row !! col)

-- Get all trees in the same row, split into the before and after (and excluding the specified tree)
treesInRow :: ((Int, Int), Int) -> [((Int, Int), Int)] -> ([((Int, Int), Int)], [((Int, Int), Int)])
treesInRow ((x, y), z) = partition (\((x2, _), _) -> x2 < x) . filter (\((x2, y2), _) -> y2 == y && x2 /= x)

-- Get all trees in the same column, split into the before and after (and excluding the specified tree)
treesInColumn :: ((Int, Int), Int) -> [((Int, Int), Int)] -> ([((Int, Int), Int)], [((Int, Int), Int)])
treesInColumn ((x, y), z) = partition (\((_, y2), _) -> y2 < y) . filter (\((x2, y2), _) -> x2 == x && y2 /= y)

-- Gets the height of the trees in the list
height :: ((Int, Int), Int) -> Int
height ((_, _), height) = height

-- Gets the tallest tree height for both partitions and uses that to compare against the given tree to determine visibility
isVisibleBeforeAfter :: ((Int, Int), Int) -> ([((Int, Int), Int)], [((Int, Int), Int)]) -> Bool
isVisibleBeforeAfter tree (before, after) = (height tree) > maximum ([-1]++(map height before)) || (height tree) > maximum ([-1]++(map height after))

-- Given the forest map, determine if each tree is visible
mapVisibility :: [((Int, Int), Int)] -> [(((Int, Int), Int), Bool)]
mapVisibility forest = map (\tree -> (tree, isVisibleBeforeAfter tree (treesInRow tree forest) || isVisibleBeforeAfter tree (treesInColumn tree forest))) forest

-- Format a visibility map for printing
formatTreeVisibility :: (((Int, Int), Int), Bool) -> String
formatTreeVisibility (((x, y), z), visible) = "(" ++ show x ++ ", " ++ show y ++ ") has height of " ++ show z ++ " and " ++ if visible then "is Visible" else "is Hidden"

-- Gets the farthest tree visible from the given tree in each direction
farthestVisibleBeforeAfter :: ((Int, Int), Int) -> ([((Int, Int), Int)], [((Int, Int), Int)]) -> (((Int, Int), Int), ((Int, Int), Int))
farthestVisibleBeforeAfter tree (before, after) = (last (takeWhile (\seen -> (height seen) >= (height tree)) (reverse before)), last (takeWhile (\seen -> (height seen) >= (height tree)) after))
