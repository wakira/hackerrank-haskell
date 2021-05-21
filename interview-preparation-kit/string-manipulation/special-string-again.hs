module Main where

import qualified Data.List as L
import Data.Semigroup
import System.Environment
import System.IO

-- Complete the substrCount function below.
substrCount :: Int -> String -> Int
substrCount n s =
    let
        grouped = L.group s :: [[Char]]
        numCombsSameChar = getSum $ foldMap (Sum . (\x -> (1+x)*x `div` 2) . length) grouped
        triplets = zip3 grouped (tail grouped) (tail . tail $ grouped) :: [([Char], [Char], [Char])]

        validsInTriplet :: ([Char], [Char], [Char]) -> Int
        validsInTriplet (a:as, b, c:cs) | length b == 1 && a == c = min (length (a:as)) (length (c:cs))
        validsInTriplet (a:as, b, c:cs) = 0
    in
        numCombsSameChar + getSum (foldMap (Sum . validsInTriplet) triplets)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Int

    s <- getLine

    let result = substrCount n s

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
