module Main where

import System.Environment
import System.IO

--
-- Complete the 'makeAnagram' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. STRING a
--  2. STRING b
--

makeAnagram a b = do
    -- Write your code here
    foldr step 0 ['a' .. 'z'] where
        step c acc = acc + abs (length (filter (==c) a) - length (filter (==c) b))

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    a <- getLine

    b <- getLine

    let res = makeAnagram a b

    hPutStrLn fptr $ show res

    hFlush fptr
    hClose fptr
