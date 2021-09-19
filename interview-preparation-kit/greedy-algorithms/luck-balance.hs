module Main where

import Data.Semigroup
import qualified Data.List
import qualified Data.Text
import System.Environment
import System.IO

--
-- Complete the 'luckBalance' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER k
--  2. 2D_INTEGER_ARRAY contests
--

luckBalance k contests =
    -- Write your code here
    let
        (important, unimportant) = foldr (\[a,b] (im,un) -> if b == 1 then (a:im,un) else (im,a:un)) ([],[]) contests
        importantSorted = Data.List.sort important
        (importantWins, importantLoses) = splitAt (length importantSorted - k) importantSorted
    in
        getSum (foldMap Sum importantLoses) + getSum (foldMap Sum unimportant) - getSum (foldMap Sum importantWins)

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

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let k = read (firstMultipleInput !! 1) :: Int

    contestsTemp <- readMultipleLinesAsStringArray n
    let contests = Data.List.map (\x -> Data.List.map (read :: String -> Int) . Data.List.words $ rstrip x) contestsTemp

    let result = luckBalance k contests

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
