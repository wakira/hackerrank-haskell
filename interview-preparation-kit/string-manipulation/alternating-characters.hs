module Main where

import Control.Monad
import qualified Data.Text
import System.Environment
import System.IO

--
-- Complete the 'alternatingCharacters' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts STRING s as parameter.
--

alternatingCharacters =
    snd . foldr (\a (pre,acc) -> if a==pre then (a,acc+1) else (a,acc)) ('X',0)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    forM_ [1..q] $ \q_itr -> do
        s <- getLine

        let result = alternatingCharacters s

        hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
