{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad ( forM_ )
import Data.List ( map )
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as B
import System.Environment ( getEnv )
import System.IO
    ( IOMode(WriteMode), hClose, hFlush, openFile, hPutStrLn )


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

solve arr fptr = do
  let (sorted, invs) = msort arr
  hPutStrLn fptr $ "Array is sorted in " <> show invs <> " swaps."
  hPutStrLn fptr $ "First Element: " <> show (head sorted)
  hPutStrLn fptr $ "Last Element: " <> show (last sorted)

-- The default template provided by HackerRank uses String IO
-- which will cost more time in reading than the algorithm itself
-- changed to ByteString.Char8 for better performance
main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    _ <- B.getLine

    arrTemp <- B.getLine

    let arr = Data.List.map (fst . fromJust . B.readInt) . B.words $ arrTemp

    solve arr fptr

    hFlush fptr
    hClose fptr
