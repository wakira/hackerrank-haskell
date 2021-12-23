module Main where

import Control.Monad
import Data.Char (toUpper, isLower)
import Data.Array
import Data.Text
import Debug.Trace
import System.Environment
import System.IO

--
-- Complete the 'abbreviation' function below.
--
-- The function is expected to return a STRING.
-- The function accepts following parameters:
--  1. STRING a
--  2. STRING b
--

abbreviation aLs bLs =
  -- Write your code here
  if f lastIdxA lastIdxB then "YES" else "NO"
  where
    a = listArray (1, Prelude.length aLs) aLs
    b = listArray (1, Prelude.length bLs) bLs

    lastIdxA = snd $ bounds a
    lastIdxB = snd $ bounds b

    matIdx (i, j) = i*(lastIdxB + 1) + j

    f 0 0 = True
    f i 0 = isLower (a ! i) && (cache ! matIdx (i-1, 0))
    f 0 j = False
    f i j = if isLower (a ! i)
               then if Data.Char.toUpper (a ! i) == (b ! j)
                       then cache ! matIdx (i-1, j-1) || cache ! matIdx (i-1, j)
                       else cache ! matIdx (i-1, j)
               else (a ! i) == (b ! j) && cache ! matIdx (i-1, j-1)

    cache = listArray (0, matIdx (lastIdxA, lastIdxB)) [f i j | i <- [0.. lastIdxA], j <- [0..lastIdxB]]

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack

rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  qTemp <- getLine
  let q = read $ lstrip $ rstrip qTemp :: Int

  forM_ [1 .. q] $ \q_itr -> do
    a <- getLine

    b <- getLine

    let result = abbreviation a b

    hPutStrLn fptr result

  hFlush fptr
  hClose fptr
