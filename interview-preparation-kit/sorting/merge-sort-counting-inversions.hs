{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad ( forM_ )
import Data.List ( map )
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as B
import System.Environment ( getEnv )
import System.IO
    ( IOMode(WriteMode), hClose, hFlush, openFile, hPrint )


merge :: Ord a => [a] -> [a] -> ([a], Int)
merge xs ys = helper xs ys (length xs)
    where
        helper [] ys _ = (ys, 0)
        helper xs [] _ = (xs, 0)
        helper (x:xs) (y:ys) xsUsed | x <= y = (x:merged, wtf)
            where
                (merged, wtf) = helper xs (y:ys) (xsUsed - 1)
        helper (x:xs) (y:ys) xsUsed = (y:merged, wtf + xsUsed)
            where
                (merged, wtf) = helper (x:xs) ys xsUsed

msort :: Ord a => [a] -> ([a], Int)
msort [] = ([], 0)
msort [x] = ([x], 0)
msort xs = (mergeSorted, leftInvs + rightInvs + acrossInvs)
    where
        (left, right) = splitAt (length xs `div` 2) xs
        (leftSorted, leftInvs) = msort left
        (rightSorted, rightInvs) = msort right
        (mergeSorted, acrossInvs) = merge leftSorted rightSorted

-- Complete the countInversions function below.
countInversions :: [Int] -> Int
countInversions arr = snd $ msort arr

-- The default template provided by HackerRank uses String IO
-- which will cost more time in reading than the algorithm itself
-- changed to ByteString.Char8 for better performance
main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        _ <- B.getLine

        arrTemp <- B.getLine

        let arr = Data.List.map (fst . fromJust . B.readInt) . B.words $ arrTemp

        let result = countInversions arr

        hPrint fptr result

    hFlush fptr
    hClose fptr
