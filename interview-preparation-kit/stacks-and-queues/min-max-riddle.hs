module Main where

import Control.Monad.ST
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Control.Monad ( when )
import Data.Foldable ( for_ )
import Data.Maybe ( fromJust )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Prelude hiding (length, (!))

type Idx = Int

go :: V.Vector Int -> Idx -> [(Int, Idx)] -> MV.MVector s Idx -> MV.MVector s Int -> ST s (MV.MVector s Int)
go arr i stack toTheLeft result | i == V.length arr = pure result
go arr i stack toTheLeft result = do
  let h = arr V.! i
  -- pop until stack top < h; for each popped (v, idx), the range is [idx + 1, i - 1]
  let (largerEquals, smallers) = Prelude.break ((< h) . fst) stack
  let fstSmallerIdx = case smallers of
        [] -> -1
        ((_, i):_) -> i
  -- record the index of the first lesser value to the left
  MV.write toTheLeft i fstSmallerIdx
  -- for each value popped from stack, update the result
  for_ largerEquals $ \(v, idx) -> do
    left <- MV.read toTheLeft idx
    let spanIdx = i - left - 2 -- spanIdx is span - 1
    for_ [0..spanIdx] $ \j -> do
      curMax <- MV.read result j
      when (v > curMax) (MV.write result j v)
  go arr (i + 1) ((h, i):smallers) toTheLeft result

-- Complete the riddle function below.
riddle :: V.Vector Int -> [Int]
riddle arr = do
  -- complete this function
  V.toList $ runST $ do
    let extendedArr = V.generate (V.length arr + 1) $ \i -> if i == V.length arr then 0 else arr V.! i
    r <- MV.new (V.length arr)
    toTheLeft <- V.unsafeThaw $ V.replicate (V.length extendedArr) 0
    result <- go extendedArr 0 [] toTheLeft r
    V.unsafeFreeze result

main :: IO()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  n <- readLn :: IO Int

  arrTemp <- B.getLine

  let arr = V.fromList $ Prelude.map (fst . fromJust . B.readInt) . B.words $ arrTemp

  let res = riddle arr

  hPutStrLn fptr $ unwords $ Prelude.map show res

  hFlush fptr
  hClose fptr
