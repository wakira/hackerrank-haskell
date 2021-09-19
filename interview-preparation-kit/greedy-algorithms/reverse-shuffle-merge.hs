{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), Map)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

--
-- Complete the 'reverseShuffleMerge' function below.
--
-- The function is expected to return a STRING.
-- The function accepts STRING s as parameter.
--

charCnt = foldr step Map.empty
  where
    step c counter = Map.insertWith (+) c 1 counter

use x = Map.insertWith (+) x (-1)
deuse x = Map.insertWith (+) x 1

reverseShuffleMerge :: String -> String
reverseShuffleMerge a =
  let
    charRemain = charCnt a
    origCharRemain = Map.map (`div` 2) charRemain

    tryReplace :: Char -> [Char] -> Map Char Int -> Map Char Int -> ([Char], Map Char Int)
    tryReplace x [] cr ocr = ([], ocr)
    tryReplace x (s:ss) cr ocr =
      if x < s && cr ! s >= ocr ! s + 1
        then tryReplace x ss cr (deuse s ocr)
        else (s:ss, ocr)

    go [] s _ _ = s
    go (x:xs) s cr ocr
      | ocr ! x == 0 = -- x is from shuffle(A)
        go xs s (use x cr) ocr
      | null s || x >= head s =
        go xs (x:s) (use x cr) (use x ocr)
      | x < head s && cr ! head s >= ocr ! head s + 1 =
        let
          (sAfter, ocrAfter) = tryReplace x s cr ocr
        in
          go xs (x:sAfter) (use x cr) (use x ocrAfter)
      | otherwise =
         go xs (x:s) (use x cr) (use x ocr)

  in
    reverse $ go (reverse a) [] charRemain origCharRemain

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  s <- getLine

  let result = reverseShuffleMerge s

  hPutStrLn fptr result

  hFlush fptr
  hClose fptr
