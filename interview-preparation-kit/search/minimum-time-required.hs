module Main where

import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as B
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import System.Environment ( getEnv )
import Data.Semigroup (Sum(..), getSum)
import System.IO
    ( hClose, hFlush, openFile, hPutStrLn, IOMode(WriteMode) )
import Debug.Trace

productAtTime t intProdMap =
  getSum $ Map.foldMapWithKey (\int num -> Sum (t `div` int * num)) intProdMap

-- Complete the minTime function below.
minTime machines goal =
  let
    -- intProdMap is a Map from production time to number of machines
    intProdMap = foldl' (\b a -> Map.insertWith (+) a 1 b) Map.empty machines
    fastestTime = fst $ Map.findMin intProdMap
    slowestTime = fst $ Map.findMax intProdMap
    nMachines = length machines
    -- searchMin is the time assuming all machines are as fast as the fastest machine
    searchMin = floor $ fromIntegral goal * fromIntegral fastestTime / fromIntegral nMachines
    -- searchMax is the time assuming all machines are as slow as the slowest machine
    searchMax = ceiling $ fromIntegral goal * fromIntegral slowestTime / fromIntegral nMachines

    search smin smax | smin < smax =
      let
        smid = smin + (smax - smin) `div` 2
        productAtSMid = productAtTime smid intProdMap
      in
        traceShow smin $ traceShow smax $
        if productAtSMid >= goal
        then search smin smid
        else search (smid + 1) smax
    search smin smax = smin
  in
    search searchMin searchMax

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

  nGoalTemp <- getLine
  let nGoal = words nGoalTemp

  let n = read (nGoal !! 0) :: Int

  let goal = read (nGoal !! 1) :: Int

  machinesTemp <- B.getLine

  let machines = map (fst . fromJust . B.readInt) $ B.words machinesTemp

  let ans = minTime machines goal

  hPutStrLn fptr $ show ans

  hFlush fptr
  hClose fptr
