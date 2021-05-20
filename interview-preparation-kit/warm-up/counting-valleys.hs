module Main where

import System.Environment
import Data.Text
import System.IO
import Control.Monad.State
import Data.Functor.Identity (Identity)

type ValleyCount a = StateT Int Identity a

type SeaLevel = Int

type Move = Char

hikerMove :: SeaLevel -> Move -> ValleyCount SeaLevel
hikerMove l 'U' | l == -1 = do
    modify (+1)
    return $ 0
hikerMove l 'D' = return $ l - 1
hikerMove l 'U' | l /= -1 = return $ l + 1

--
-- Complete the 'countingValleys' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER steps
--  2. STRING path
--

countingValleys :: Int -> String -> Int
countingValleys steps path =
    execState (foldM (\level move -> hikerMove level move) 0 path) 0
    -- Write your code here

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    stepsTemp <- getLine
    let steps = read $ lstrip $ rstrip stepsTemp :: Int

    path <- getLine

    let result = countingValleys steps path

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
