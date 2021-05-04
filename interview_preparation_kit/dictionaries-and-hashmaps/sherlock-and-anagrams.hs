module Main where

import Control.Monad
import Data.List
import System.Environment
import System.IO
import qualified Data.Map.Strict as Map


-- Complete the sherlockAndAnagrams function below.
sherlockAndAnagrams s =
    let
        sortedSubStrs = Prelude.map Data.List.sort $ do
            dropped <- [ Prelude.drop i s | i <- [0..length s - 1]]
            [Prelude.take j dropped | j <- [1..length dropped]]
        step s (m, tot) = case Map.lookup s m of
            Nothing -> (Map.insert s 1 m, tot)
            Just num -> (Map.insertWith (+) s 1 m, tot + num)
    in
        snd $ Prelude.foldr step (Map.empty, 0) sortedSubStrs


main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    q <- readLn :: IO Int

    forM_ [1..q] $ \q_itr -> do
        s <- getLine

        let result = sherlockAndAnagrams s

        hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
