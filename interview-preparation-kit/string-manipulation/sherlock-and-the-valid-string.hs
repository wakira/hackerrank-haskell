module Main where

import Data.Map.Strict as M
import qualified Data.List as L (sort, group)
import System.Environment
import System.IO

--
-- Complete the 'isValid' function below.
--
-- The function is expected to return a STRING.
-- The function accepts STRING s as parameter.
--

isValid s =
    -- Write your code here
    let
        charCnt = Prelude.foldr (\v m -> insertWith (+) v 1 m) empty s
        cnts = L.sort $ M.elems charCnt
        grouped = L.group cnts :: [[Int]]
        fstGroup = head grouped :: [Int]
        -- note that the following code do not error when grouped's length is 1
        -- because of Haskell's lazy evaluation
        sndGroup = head . tail $ grouped :: [Int]
        cntInFstGroup = head fstGroup :: Int
        cntInSndGroup = head sndGroup :: Int
    in
        if length grouped == 1 || -- all the same
           (length grouped == 2 && length fstGroup == 1 && cntInFstGroup == 1) ||
           (length grouped == 2 && length sndGroup == 1 && cntInSndGroup == cntInFstGroup + 1)
        then "YES" else "NO"

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    let result = isValid s

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
