module Main where

import Data.List (map, intercalate)
import System.Environment
import System.IO

-- Complete the rotLeft function below.
rotLeft a d =
    let
        aLen = length a
        dShift = d `mod` aLen
    in
        take aLen $ drop dShift $ cycle a

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

    ndTemp <- getLine
    let nd = words ndTemp

    let n = read (nd !! 0) :: Int

    let d = read (nd !! 1) :: Int

    aTemp <- getLine

    let a = Data.List.map (read :: String -> Int) . words $ aTemp

    let result = rotLeft a d

    hPutStrLn fptr $ intercalate " " $ Data.List.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr
