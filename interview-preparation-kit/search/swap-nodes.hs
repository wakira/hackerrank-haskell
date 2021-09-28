module Main where

import qualified Data.Map.Strict as Map
import Data.List
import Data.Text
import System.Environment
import System.IO

--
-- Complete the 'swapNodes' function below.
--
-- The function is expected to return a 2D_INTEGER_ARRAY.
-- The function accepts following parameters:
--  1. 2D_INTEGER_ARRAY indexes
--  2. INTEGER_ARRAY queries
--

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

buildTree :: Map.Map Int [Int] -> Tree Int
buildTree childMap =
  go 1
  where
    go i = case childMap Map.! i of
      [-1, -1] -> Node i EmptyTree EmptyTree
      [-1, r] -> Node i EmptyTree (go r)
      [l, -1] -> Node i (go l) EmptyTree
      [l, r] -> Node i (go l) (go r)
      _ -> error "Impossible"


swapTree :: Int -> Tree a -> Tree a
swapTree level =
  go 1
  where
    go _ EmptyTree = EmptyTree
    go currentLvl (Node v l r) =
      if currentLvl `mod` level == 0
      then Node v (go (currentLvl + 1) r) (go (currentLvl + 1) l)
      else Node v (go (currentLvl + 1) l) (go (currentLvl + 1) r)

traverseTree :: Tree a -> [a]
traverseTree EmptyTree = []
traverseTree (Node v l r) = traverseTree l ++ (v : traverseTree r)

swapNodes :: [[Int]] -> [Int] -> [[Int]]
swapNodes indexes queries =
  -- Write your code here
  let
    root = buildTree $ Map.fromAscList $ Data.List.zip [1..] indexes

    go [] _ = []
    go (q:qs) r =
      let
        swapped = swapTree q r
      in
        (traverseTree swapped : go qs swapped)
  in
    go queries root


lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack

rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
  line <- getLine
  rest <- readMultipleLinesAsStringArray (n - 1)
  return (line : rest)

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  nTemp <- getLine
  let n = read $ lstrip $ rstrip nTemp :: Int

  indexesTemp <- readMultipleLinesAsStringArray n
  let indexes = Data.List.map (\x -> Data.List.map (read :: String -> Int) . Data.List.words $ rstrip x) indexesTemp

  queriesCountTemp <- getLine
  let queriesCount = read $ lstrip $ rstrip queriesCountTemp :: Int

  queriesTemp <- readMultipleLinesAsStringArray queriesCount
  let queries = Data.List.map (read :: String -> Int) queriesTemp

  let result = swapNodes indexes queries

  hPutStrLn fptr $ Data.List.intercalate "\n" $ Data.List.map (\x -> Data.List.intercalate " " $ Data.List.map (\y -> show y) $ x) $ result

  hFlush fptr
  hClose fptr
