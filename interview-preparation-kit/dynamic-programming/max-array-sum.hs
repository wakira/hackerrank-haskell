module Main where

import Data.Array
import Data.List
import System.Environment
import System.IO

-- Complete the maxSubsetSum function below.
maxSubsetSum arr = s lastArrIdx
  where
    lastArrIdx = snd $ bounds arr
    s (-1) = 0
    s 0 = 0
    s i = max (cache ! (i-1)) (arr ! i + cache ! (i-2))
    cache = listArray (-1, lastArrIdx) [s i | i <- [-1..lastArrIdx]]

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  n <- readLn :: IO Int

  arrTemp <- getLine

  let lst = Data.List.map (read :: String -> Int) . words $ arrTemp
  let arr = listArray (1, length lst) lst

  let res = maxSubsetSum arr

  hPrint fptr res

  hFlush fptr
  hClose fptr
