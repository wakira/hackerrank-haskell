module Main where

import qualified Data.Map.Strict as Map
import Data.List
import Data.Text
import System.Environment
import System.IO

--
-- Complete the 'pairs' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER k
--  2. INTEGER_ARRAY arr
--

pairs k arr =
  go Map.empty arr 0
  -- Write your code here
  where
    go _ [] cnt = cnt
    go m (x:xs) cnt =
      go (Map.insertWith (+) x 1 m) xs
      (cnt + Map.findWithDefault 0 (x + k) m + Map.findWithDefault 0 (x - k) m)


lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack

rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  firstMultipleInputTemp <- getLine
  let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

  let n = read (firstMultipleInput !! 0) :: Int

  let k = read (firstMultipleInput !! 1) :: Int

  arrTemp <- getLine

  let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp

  let result = pairs k arr

  hPutStrLn fptr $ show result

  hFlush fptr
  hClose fptr
