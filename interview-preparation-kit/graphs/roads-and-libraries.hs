module Main where

import Control.Monad (forM_)
import Data.List (map, words)
import Data.Text (pack, stripEnd, stripStart, unpack)
import System.Environment (getEnv)
import System.IO
  ( IOMode (WriteMode),
    hClose,
    hFlush,
    hPutStrLn,
    openFile,
  )
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Vertex = Int
type RoadInfo = M.Map Vertex [Vertex] -- an array is better when not enforcing immutability

--
-- Complete the 'roadsAndLibraries' function below.
--
-- The function is expected to return a LONG_INTEGER.
-- The function accepts following parameters:
--  1. INTEGER n
--  2. INTEGER c_lib
--  3. INTEGER c_road
--  4. 2D_INTEGER_ARRAY cities
--

vertexNeighbors = M.findWithDefault []

connectedComponents :: Int -> RoadInfo -> [[Vertex]]
connectedComponents n roads = go 1 roads (S.singleton 1) [1] [] []
  where
    go v roads visited stack currentComponents foundComponents | v > n = currentComponents:foundComponents
    go v roads visited stack currentComponents foundComponents =
      case stack of
        [] -> if v `S.member` visited
          then go (v + 1) roads visited stack currentComponents foundComponents
          else go v roads (S.insert v visited) [v] [] (currentComponents:foundComponents)
        s:sx ->
          -- mark and visit all neighbors
          let
            neighbors = vertexNeighbors s roads
            unvisitedNeighbors = filter (not . (`S.member` visited)) neighbors
            visited' = foldr S.insert visited unvisitedNeighbors
          in
            go v roads visited' (unvisitedNeighbors ++ sx) (s:currentComponents) foundComponents

roadsAndLibraries n c_lib c_road cities =
  -- Write your code here
  if c_lib <= c_road
    then n * c_lib
    else let
      roads = makeRoadInfo cities
      components = connectedComponents n roads
      componentSizes = map length components
    in
      sum $ map (\s -> (s - 1) * c_road + c_lib) componentSizes

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
  line <- getLine
  rest <- readMultipleLinesAsStringArray (n - 1)
  return (line : rest)

makeRoadInfo :: [[Int]] -> RoadInfo
makeRoadInfo = foldr step M.empty
  where
    step [cityA, cityB] m =
      M.insertWith (++) cityB [cityA] $ M.insertWith (++) cityA [cityB] m

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  qTemp <- getLine
  let q = read qTemp :: Int

  forM_ [1 .. q] $ \q_itr -> do
    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let m = read (firstMultipleInput !! 1) :: Int

    let c_lib = read (firstMultipleInput !! 2) :: Int

    let c_road = read (firstMultipleInput !! 3) :: Int

    citiesTemp <- readMultipleLinesAsStringArray m
    let cities = Data.List.map (\x -> Data.List.map (read :: String -> Int) . Data.List.words $ x) citiesTemp

    let result = roadsAndLibraries n c_lib c_road cities

    hPutStrLn fptr $ show result

  hFlush fptr
  hClose fptr
