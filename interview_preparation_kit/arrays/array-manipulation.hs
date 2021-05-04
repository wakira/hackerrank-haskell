module Main where

import Control.Monad
import Data.List
import System.Environment
import System.IO
import Data.Map.Strict as M
import Data.Semigroup

-- Complete the arrayManipulation function below.
arrayManipulation :: Int -> [[Int]] -> Int
arrayManipulation n queries =
    let
        opMap = Prelude.foldr (\[a,b,k] m -> M.insertWith (+) a k
                                        (M.insertWith (+) (b+1) (-k) m))
                     M.empty queries
        opList = M.toList opMap
    in
        getMax $ foldMap Max $ scanl (\tot (_, op) -> tot + op) 0 opList


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

    nmTemp <- getLine
    let nm = words nmTemp

    let n = read (nm !! 0) :: Int

    let m = read (nm !! 1) :: Int

    queriesTemp <- readMultipleLinesAsStringArray m
    let queries = Data.List.map (\x -> Data.List.map (read :: String -> Int) . words $ x) queriesTemp

    let result = arrayManipulation n queries

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
