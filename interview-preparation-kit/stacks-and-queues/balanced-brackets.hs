module Main where

import Control.Monad
import Data.Text
import qualified Data.List as List
import System.Environment
import System.IO

--
-- Complete the 'isBalanced' function below.
--
-- The function is expected to return a STRING.
-- The function accepts STRING s as parameter.
--

isBalanced s = if go s [] then "YES" else "NO"
  where
    pairOf ')' = '('
    pairOf ']' = '['
    pairOf '}' = '{'

    go [] [] = True
    go [] _ = False
    go ('(':cs) stack = go cs ('(':stack)
    go ('[':cs) stack = go cs ('[':stack)
    go ('{':cs) stack = go cs ('{':stack)
    go (c:cs) stack = case List.break (== pairOf c) stack of
      (_, []) -> False
      (revInside, match:remainder) ->  go (Prelude.reverse revInside) [] && go cs remainder

-- Write your code here
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack

rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  tTemp <- getLine
  let t = read $ lstrip $ rstrip tTemp :: Int

  forM_ [1 .. t] $ \t_itr -> do
    s <- getLine

    let result = isBalanced s

    hPutStrLn fptr result

  hFlush fptr
  hClose fptr
