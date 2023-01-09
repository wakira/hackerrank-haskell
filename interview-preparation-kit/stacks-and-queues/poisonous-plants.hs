module Main where

import Control.Monad.ST (ST, runST)
import Data.Maybe (fromJust)
import Data.List (map, words)
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import System.Environment (getEnv)
import System.IO
  ( IOMode (WriteMode),
    hClose,
    hFlush,
    hPrint,
    openFile,
  )

--
-- Complete the 'poisonousPlants' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts INTEGER_ARRAY p as parameter.
--

go :: [Int] -> Int -> [(Int, Int)] -> MV.MVector s Int -> Control.Monad.ST.ST s (MV.MVector s Int)
go [] pos stack result = pure result
go (x : xs) pos [] result =
  -- stack is empty (when processing first element)
  MV.write result pos 0 >> go xs (pos + 1) [(pos, x)] result
go (x : xs) pos stack result =
  -- normal case
  let (largerEquals, smallers) = Prelude.break ((< x) . snd) stack
   in case smallers of
        [] -> MV.write result pos 0 >> go xs (pos + 1) ((pos, x) : smallers) result
        ((fstSmallerIdx, _) : _) ->
          if fstSmallerIdx + 1 == pos
            then MV.write result pos 1 >> go xs (pos + 1) ((pos, x) : smallers) result
            else do
              prevMaxRounds <- maximum <$> sequence [MV.read result i | (i, _) <- largerEquals]
              MV.write result pos (prevMaxRounds + 1)
              go xs (pos + 1) ((pos, x) : smallers) result

poisonousPlants :: [Int] -> Int
poisonousPlants p =
  -- Write your code here
  V.maximum $ Control.Monad.ST.runST $ do
    r <- MV.new (length p)
    result <- go p 0 [] r
    V.unsafeFreeze result

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  n <- readLn :: IO Int

  pTemp <- B.getLine
  let p = Prelude.map (fst . fromJust . B.readInt) . B.words $ pTemp

  let result = poisonousPlants p

  hPrint fptr result

  hFlush fptr
  hClose fptr
