module Main where

import Control.Monad
import qualified Data.Set as Set
import System.Environment
import System.IO

-- Complete the twoStrings function below.
twoStrings :: String -> String -> String
twoStrings s1 s2 =
    if any (\c -> Set.member c (Set.fromList s1)) s2 then "YES" else "NO"

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    q <- readLn :: IO Int

    forM_ [1..q] $ \q_itr -> do
        s1 <- getLine

        s2 <- getLine

        let result = twoStrings s1 s2

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr
