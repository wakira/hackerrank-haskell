module Main where

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Control.Monad (forM_)

--
-- Complete the 'whatFlavors' function below.
--
-- The function accepts following parameters:
--  1. INTEGER_ARRAY cost
--  2. INTEGER money
--

whatFlavors :: [Int] -> Int -> IO ()
whatFlavors cost money = do
  -- Write your code here
  let (p, c) = go 1 Map.empty cost
  putStrLn $ show p <> " " <> show c
  where
    go idx seen (x:xs) = case x `Map.lookup` seen of
      Just prev_idx -> (prev_idx, idx)
      Nothing -> go (idx + 1) (Map.insert (money - x) idx seen) xs
    go _ _ [] = error "Impossible"

main :: IO ()
main = do
  t <- read <$> getLine

  forM_ [1 .. t] $ \t_itr -> do
    money <- read <$> getLine

    n <- read <$> getLine :: IO Int

    cost <- map (fst . fromJust . B.readInt) . B.words <$> B.getLine

    whatFlavors cost money
