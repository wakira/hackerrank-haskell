{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.IntSet as IS
import qualified Data.Vector as V -- might better use Array for multi-dimension situation like this
import qualified Data.Sequence as S
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPrint, openFile, IOMode(WriteMode) )
import Debug.Trace

maxN = 100

newtype Position = Position (Int, Int)
  deriving (Ord, Eq, Show)

newtype Visited = Visited IS.IntSet

visited (Position (x, y)) (Visited is) = IS.member (x * maxN + y) is

visit (Position (x, y)) (Visited is) = Visited $ IS.insert (x * maxN + y) is

type Passable = IS.IntSet
passable (Position (x, y)) = IS.notMember (x * maxN + y)

--
-- Complete the 'minimumMoves' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. STRING_ARRAY grid
--  2. INTEGER startX
--  3. INTEGER startY
--  4. INTEGER goalX
--  5. INTEGER goalY
--

findTargets n grid v (Position (x, y)) =
  let
    up = reverse $ takeWhile (`passable` grid) [Position (i, y) | i <- reverse [0..x - 1], not (visited (Position (i, y)) v)]
    down = reverse $ takeWhile (`passable` grid) [Position (i, y) | i <- [x..n - 1], not (visited (Position (i, y)) v)]
    left = reverse $ takeWhile (`passable` grid) [Position (x, j) | j <- reverse [0..y - 1], not (visited (Position (x, j)) v)]
    right = reverse $ takeWhile (`passable` grid) [Position (x, j) | j <- [y + 1..n - 1], not (visited (Position (x, j)) v)]
  in
    up ++ down ++ left ++ right

bfs :: Int -> Passable -> Position -> Visited -> S.Seq (Int, Position) -> Int
bfs n grid goal v queue =
  case S.viewl queue of
    S.EmptyL -> error "Cannot reach the goal"
    -- (cm, q) S.:< qs -> traceShow q $
    (cm, q) S.:< qs ->
      if q == goal then cm
                   -- else traceShow targets $ bfs grid goal v' queue'
                   else bfs n grid goal v' queue'
                     where v' = Prelude.foldr visit v targets
                           queue' = qs S.>< S.fromList (map (cm + 1,) targets)
                           targets = findTargets n grid v q

minimumMoves grid startX startY goalX goalY n =
  -- Write your code here
  bfs n grid (Position (goalX, goalY)) (visit (Position (startX, startY)) (Visited IS.empty)) (S.singleton (0, Position (startX, startY)))

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
  line <- getLine
  rest <- readMultipleLinesAsStringArray(n - 1)
  return (line : rest)

convertToVec :: [[Char]] -> Passable
convertToVec lst =
  let
    indexed = concatMap (\(i, line) -> [(i, j, v) | (j, v) <- line]) $ zip [0..] $ map (zip [0..]) lst
    step (i, j, v) b = if v == 'X' then IS.insert (i * maxN + j) b else b
  in
    foldr step IS.empty indexed

main :: IO()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  nTemp <- getLine
  let n = read nTemp :: Int

  grid <- readMultipleLinesAsStringArray n

  firstMultipleInputTemp <- getLine
  let firstMultipleInput = words firstMultipleInputTemp

  let startX = read (head firstMultipleInput) :: Int

  let startY = read (firstMultipleInput !! 1) :: Int

  let goalX = read (firstMultipleInput !! 2) :: Int

  let goalY = read (firstMultipleInput !! 3) :: Int

  let gridVec = convertToVec grid

  let result = minimumMoves gridVec startX startY goalX goalY n

  hPrint fptr result

  hFlush fptr
  hClose fptr
