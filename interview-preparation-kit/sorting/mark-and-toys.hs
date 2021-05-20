module Main where

import Data.List
import System.Environment
import System.IO

-- Complete the maximumToys function below.
maximumToys prices k =
    length (Prelude.takeWhile (<=k) $ Data.List.scanl (+) 0 (Data.List.sort prices)) - 1

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nkTemp <- getLine
    let nk = words nkTemp

    let n = read (nk !! 0) :: Int

    let k = read (nk !! 1) :: Int

    pricesTemp <- getLine

    let prices = Data.List.map (read :: String -> Int) . words $ pricesTemp

    let result = maximumToys prices k

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
