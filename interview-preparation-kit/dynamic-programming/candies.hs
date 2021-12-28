module Main where

import Data.Array
import Data.List
import Data.Text
import System.Environment
import System.IO

--
-- Complete the 'candies' function below.
--
-- The function is expected to return a LONG_INTEGER.
-- The function accepts following parameters:
--  1. INTEGER n
--  2. INTEGER_ARRAY arr
--

candies n arr =
  -- Write your code here
  sum [c i | i <- [1..n]]
  where
    a = listArray (0, n + 1) (0:arr <> [0])

    c i | i == 0 = 0
        | i == n + 1 = 0
        | a ! (i-1) < a ! i && a ! i > a ! (i+1) = max (cache ! (i-1)) (cache ! (i+1)) + 1
        | a ! (i-1) >= a ! i && a ! i <= a ! (i+1) = 1
        | a ! (i-1) < a ! i && a ! i <= a ! (i+1) = (cache ! (i-1)) + 1
        | a ! (i-1) >= a ! i && a ! i > a ! (i+1) = (cache ! (i+1)) + 1
        | otherwise = error "impossible"
    cache = listArray (0, n + 1) [ c i | i <- [0..n + 1]]

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

  arrTemp <- readMultipleLinesAsStringArray n
  let arr = Data.List.map (read :: String -> Int) arrTemp

  let result = candies n arr

  hPrint fptr result

  hFlush fptr
  hClose fptr
