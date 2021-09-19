module Main where

import GHC.Base (maxInt)
import qualified Data.List
import qualified Data.Text
import Data.Semigroup
import System.Environment
import System.IO

--
-- Complete the 'minimumAbsoluteDifference' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts INTEGER_ARRAY arr as parameter.
--

minimumAbsoluteDifference arr =
    -- Write your code here
    let
        sorted = Data.List.sort arr
        pairs = zip sorted (tail sorted)
    in
        getMin $ foldMap (\(a, b) -> Min (b-a)) pairs

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp

    let result = minimumAbsoluteDifference arr

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
