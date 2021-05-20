module Main where

import System.Environment
import System.IO

-- Complete the repeatedString function below.
repeatedString s n =
    let
        repeats = n `div` length s
        remain = n `mod` length s
        asInS = length $ filter (=='a') s :: Int
    in
        repeats * asInS + length (filter (=='a') (take remain s))

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    n <- readLn :: IO Int

    let result = repeatedString s n

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
