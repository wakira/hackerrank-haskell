module Main where

import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Control.Monad
import Data.List
import Data.Text
import System.Environment
import System.IO

--
-- Complete the 'maximumSum' function below.
--
-- The function is expected to return a LONG_INTEGER.
-- The function accepts following parameters:
--  1. LONG_INTEGER_ARRAY a
--  2. LONG_INTEGER m
--

maximumSum :: [Integer] -> Integer -> Integer
maximumSum a m =
  -- Write your code here
  let
    go [] _ _ ans = ans
    go (x:xs) acc prev ans =
      let
        curMod = (prev + x `mod` m) `mod` m
        newAcc = Set.insert curMod acc
        mbLeftMod = Set.lookupGT curMod acc
        newMax = case mbLeftMod of
                   Nothing -> max ans curMod
                   Just leftMod -> max (max ans curMod) (curMod - leftMod + m) `mod` m
      in
        go xs newAcc curMod newMax
  in
    go a Set.empty 0 0

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack

rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  qTemp <- getLine
  let q = read $ lstrip $ rstrip qTemp :: Int

  forM_ [1 .. q] $ \q_itr -> do
    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let m = read (firstMultipleInput !! 1) :: Integer

    aTemp <- getLine

    let a = Data.List.map (read :: String -> Integer) . Data.List.words $ rstrip aTemp

    let result = maximumSum a m

    hPutStrLn fptr $ show result

  hFlush fptr
  hClose fptr
