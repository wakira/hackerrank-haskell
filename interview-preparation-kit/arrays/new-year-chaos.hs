module Main where

import Control.Monad
import Data.List
import System.Environment
import System.IO
import qualified Data.Vector as V

-- Complete the minimumBribes function below.
minimumBribes :: [Int] -> IO ()
minimumBribes q =
    case V.ifoldM' step 0 vq of
        Nothing -> putStrLn "Too chaotic"
        Just t -> print t
    where
        vq = V.fromList [ i-1 | i <- q ]

        step :: Int -> Int -> Int -> Maybe Int
        step tot pos num =
            let
                sliceBeg = max 0 (num - 1)
                sliceLen = max 0 (pos - sliceBeg)
            in
                if num - pos > 2
                    then fail "early exit"
                    else pure $ tot + (V.length . (V.filter (>num)) . (V.slice sliceBeg sliceLen)) vq

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        n <- readLn :: IO Int

        qTemp <- getLine

        let q = Data.List.map (read :: String -> Int) . words $ qTemp

        minimumBribes q
