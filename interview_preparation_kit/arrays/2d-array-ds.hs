module Main where

import Data.Vector
import Data.Semigroup
import System.Environment
import System.IO

f :: Vector (Vector Int) -> Int
f v = getMax $ Prelude.foldMap Max $ Prelude.map
          (\(i, j) -> v!i!j + v!i!(j+1) + v!i!(j+2) + v!(i+1)!(j+1) + v!(i+2)!j + v!(i+2)!(j+1) + v!(i+2)!(j+2))
          [(i, j) | i <- [0..3], j <- [0..3]]

toVector :: [[a]] -> Vector (Vector a)
toVector = fromList . Prelude.map fromList

-- Complete the hourglassSum function below.
hourglassSum arr = f $ toVector arr

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

    arrTemp <- readMultipleLinesAsStringArray 6
    let arr = Prelude.map (\x -> Prelude.map (read :: String -> Int) . words $ x) arrTemp

    let result = hourglassSum arr

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
