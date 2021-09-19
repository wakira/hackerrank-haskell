module Main where

import qualified Data.List
import Data.Semigroup
import Data.List.Split (chunksOf)
import System.Environment
import System.IO

listSum = getSum . foldMap Sum

-- Complete the getMinimumCost function below.
getMinimumCost :: Int -> [Int] -> Int
getMinimumCost k c = listSum $ zipWith (\multiplier prices -> multiplier * listSum prices)
    [1..] (chunksOf k (reverse (Data.List.sort c)))

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

    nkTemp <- getLine
    let nk = words nkTemp

    let n = read (nk !! 0) :: Int

    let k = read (nk !! 1) :: Int

    cTemp <- getLine

    let c = Data.List.map (read :: String -> Int) . words $ cTemp

    let minimumCost = getMinimumCost k c

    hPutStrLn fptr $ show minimumCost

    hFlush fptr
    hClose fptr
