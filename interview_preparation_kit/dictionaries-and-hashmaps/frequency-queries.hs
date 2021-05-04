module Main where

import Data.List
import Data.Text
import System.Environment
import System.IO
import Data.Map.Strict as M

-- Complete the freqQuery function below.
freqQuery :: [[Int]] -> [Int]
freqQuery queries =
    let
        (_,_, result) = Data.List.foldl' step (M.empty, M.empty, []) queries

        step (dat, aux, res) [op, v] = case op of
            1 ->
                let
                    oldFreq = M.findWithDefault 0 v dat
                    newDat = M.insert v (oldFreq + 1) dat
                    newAux = M.adjust (\x -> max 0 (x-1)) oldFreq $
                        M.insertWith (+) (oldFreq + 1) 1 aux
                in
                    (newDat, newAux, res)
            2 ->
                case M.lookup v dat of
                    Nothing -> (dat, aux, res)
                    Just 0 -> (dat, aux, res)
                    Just oldFreq ->
                        let
                            newDat = M.insert v (oldFreq - 1) dat
                            newAux = M.adjust (\x -> max 0 (x-1)) oldFreq $
                                M.insertWith (+) (oldFreq - 1) 1 aux
                        in
                            (newDat, newAux, res)
            3 -> (dat, aux, min 1 (M.findWithDefault 0 v aux) : res)
    in
        Data.List.reverse result

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

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

    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    queriesTemp <- readMultipleLinesAsStringArray q
    let queries = Data.List.map (\x -> Data.List.map (read :: String -> Int) . Data.List.words $ rstrip x) queriesTemp

    let ans = freqQuery queries

    hPutStrLn fptr $ Data.List.intercalate "\n" $ Data.List.map (\x -> show x) $ ans

    hFlush fptr
    hClose fptr
