{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List
import System.Environment
import System.IO
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

nthInCntLst :: Int -> [(Int, Int)] -> Int
nthInCntLst n lst =  snd $ head $ snd $ Prelude.span (\(nth,num) -> nth < n) $
    Prelude.scanl (\(nth, _) (v, vcnt) -> (nth + vcnt, v)) (0, 0) lst

mediumX2 :: M.Map Int Int -> Int
mediumX2 m =
    let
        totSize = M.foldr (+) 0 m
    in
        if totSize `mod` 2 == 0
            then -- sum of mid 2
                (nthInCntLst (totSize `div` 2) (M.toAscList m)) + nthInCntLst (totSize `div` 2 + 1) (M.toAscList m)
            else -- 2x mid
                2 * (nthInCntLst (totSize `div` 2 + 1) (M.toAscList m))


cntInc :: Int -> M.Map Int Int -> M.Map Int Int
cntInc v = M.insertWith (+) v 1

cntDec :: Int -> M.Map Int Int -> M.Map Int Int
cntDec v = M.insertWith (+) v (-1)


-- Complete the activityNotifications function below.
activityNotifications :: [Int] -> Int -> Int
activityNotifications expenditure d =
    let
        recurse [] _ _ notifications = notifications
        recurse (newv: vs) (seqHead S.:<| seqRest) !movingCounter !notifications =
            if newv >= mediumX2 movingCounter
                then recurse vs (seqRest S.|> newv) (cntDec seqHead (cntInc newv movingCounter)) (notifications + 1)
                else recurse vs (seqRest S.|> newv) (cntDec seqHead (cntInc newv movingCounter)) notifications

        initialMovingList = Data.List.take d expenditure
        initialMovingCounter = Prelude.foldr cntInc M.empty initialMovingList
    in
        recurse (Data.List.drop d expenditure) (S.fromList initialMovingList) initialMovingCounter 0

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    ndTemp <- getLine
    let nd = words ndTemp

    let n = read (nd !! 0) :: Int

    let d = read (nd !! 1) :: Int

    expenditureTemp <- getLine

    let expenditure = Data.List.map (read :: String -> Int) . words $ expenditureTemp

    let result = activityNotifications expenditure d

    -- putStrLn $ show result
    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
