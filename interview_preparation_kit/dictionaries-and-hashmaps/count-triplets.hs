module Main where

import Data.List
import Data.Text
import System.Environment
import System.IO
import Data.Map.Strict as M

-- Complete the countTriplets function below.
countTriplets :: [Integer] -> Integer -> Int
countTriplets arr r =
    let
        (_, _, result) = Data.List.foldr step (M.empty, M.empty, 0) arr

        step num (tripletLast, tripletMid, tot) =
            case (M.lookup (num * r) tripletLast, M.lookup (num * r) tripletMid) of
                (Nothing, Nothing) ->
                    (M.insertWith (+) num 1 tripletLast, tripletMid, tot)
                (Just nsingle, Nothing) ->
                    (M.insertWith (+) num 1 tripletLast, M.insertWith (+) num nsingle tripletMid, tot)
                (Nothing, Just npair) -> error "Impossible"
                (Just nsingle, Just npair) ->
                    (M.insertWith (+) num 1 tripletLast, M.insertWith (+) num nsingle tripletMid, tot + npair)
    in
        result

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nrTemp <- getLine
    let nr = Data.List.words $ rstrip nrTemp

    let n = read (nr !! 0) :: Int

    let r = read (nr !! 1) :: Integer

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Integer) . Data.List.words $ rstrip arrTemp

    let ans = countTriplets arr r

    hPutStrLn fptr $ show ans

    hFlush fptr
    hClose fptr
