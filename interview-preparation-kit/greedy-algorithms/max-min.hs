module Main where

import qualified Data.List
import qualified Data.Text
import Data.Semigroup
import System.Environment
import System.IO

--
-- Complete the 'maxMin' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER k
--  2. INTEGER_ARRAY arr
--

maxMin k arr =
    -- Write your code here
    let
        sorted = Data.List.sort arr
    in
        getMin $ foldMap (\(a, b) -> Min (b-a)) $ zip sorted (drop (k-1) sorted)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    kTemp <- getLine
    let k = read $ lstrip $ rstrip kTemp :: Int

    arrTemp <- readMultipleLinesAsStringArray n
    let arr = Data.List.map (read :: String -> Int) arrTemp

    let result = maxMin k arr

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
