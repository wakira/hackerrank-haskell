module Main where

import Control.Monad
import Data.List
import System.Environment
import System.IO
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST
import Data.STRef
import Data.Function (fix)

-- Complete the minimumSwaps function below.
minimumSwaps arr =
    runST $ do
        ma <- V.unsafeThaw $ V.fromList arr
        cnt <- newSTRef 0
        forM_ [0.. VM.length ma - 2] $ \i ->
            fix $ \loop -> do
                cmp <- VM.read ma i
                if cmp /= i + 1 then do
                    VM.swap ma i (cmp - 1)
                    modifySTRef cnt (+1)
                    loop
                else return ()
        readSTRef cnt

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

    n <- readLn :: IO Int

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . words $ arrTemp

    let res = minimumSwaps arr

    hPutStrLn fptr $ show res

    hFlush fptr
    hClose fptr
