module Main where

import System.Environment
import System.IO
import qualified Data.Map.Strict as Map

-- Complete the sockMerchant function below.
sockMerchant n ar =
    let
        step color m = Map.insertWith (\nv ov -> ov+1) color 1 m
        colorCount = foldr step Map.empty ar
    in
        Map.foldr (\v tot -> tot + v `div` 2) 0 colorCount
    

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

    arTemp <- getLine

    let ar = map (read :: String -> Int) . words $ arTemp

    let result = sockMerchant n ar

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
