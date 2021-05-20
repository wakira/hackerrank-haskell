module Main where

import System.Environment
import System.IO
import System.IO.Unsafe

f ((prev2, prev1):[]) prev2Cnt prev1Cnt = prev1Cnt
f ((prev2, prev1):xs) prev2Cnt prev1Cnt =
    if prev2 == 0
    then f xs prev1Cnt (prev2Cnt + 1)
    else f xs prev1Cnt (prev1Cnt + 1)

-- Complete the jumpingOnClouds function below.
jumpingOnClouds :: [Int] -> Int
jumpingOnClouds c =
    if length c <= 3 then 1 else f (zip c (tail c)) 0 1

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

    n <- readLn :: IO Int

    cTemp <- getLine

    let c = map (read :: String -> Int) . words $ cTemp

    let result = jumpingOnClouds c

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
