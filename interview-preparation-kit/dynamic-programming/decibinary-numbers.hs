module Main where

import qualified Data.ByteString.Char8 as B
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.List ( findIndex )
import Data.Maybe ( fromJust)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unsafe ( unsafeFreeze )
import Data.Array.Unboxed
import Data.Array.IArray (Array)
import Data.Text ( stripEnd, pack, stripStart, unpack )
import System.Environment
import System.IO
    ( hClose, hFlush, openFile, hPrint, IOMode(WriteMode) )

-- a partial function that finds (previous sum, index) when the accumulative sum satisfies p
-- error when not found
findPreviousSumWithIndex p = retAns . head . dropWhile (not . p . triFst) . tail . scanl step (0, 0, -1)
  where
    triFst (a,_,_) = a
    retAns (_,b,c) = (b,c)
    step (acc, prv_acc, idx) v = (acc + v, acc, idx + 1)

-- binary search variant (x in [begIdx, endIdx], find first index that satisfies p)
findInIncreasingArrayLike p begIdx endIdx arr accessor | begIdx == endIdx = begIdx
findInIncreasingArrayLike p begIdx endIdx arr accessor | begIdx > endIdx = error "Impossible"
                                                       | otherwise =
  let
    midIdx = begIdx + (endIdx - begIdx) `div` 2
    midValue = accessor arr midIdx
  in if p midValue then findInIncreasingArrayLike p begIdx midIdx arr accessor
      else findInIncreasingArrayLike p (midIdx + 1) endIdx arr accessor

--
-- Complete the 'decibinaryNumbers' function below.
--
-- The function is expected to return a LONG_INTEGER.
-- The function accepts LONG_INTEGER x as parameter.
--

decibinaryNumbers :: [Int] -> [Int]
decibinaryNumbers queries =
  -- Write your code here
  Prelude.map solve queries
  where
    maxQueries = maximum queries
    f :: UArray (Int, Int) Int
    fSum :: UArray Int Int
    (f, fSum) = runST $ do
      f <- newArray ((0, 0), (m, n)) 0 :: ST s (STUArray s (Int, Int) Int)
      for_ [0..9] $ \i -> writeArray f (i, 0) 1
      for_ [0..n] $ \j -> writeArray f (0, j) 1
      fSum <- newArray (0, m) maxQueries :: ST s (STUArray s Int Int)
      writeArray fSum 0 1

      _ <- runMaybeT $ for_ [1..] $ \i -> do
        prevSum <- lift $ readArray fSum (i - 1)
        if prevSum >= maxQueries
          then MaybeT $ return Nothing
          else lift $
            for_ [0..n] $ \j -> do
              case (i, j) of
                (i, j) | i > 9 && j == 0 -> writeArray f (i, j) 0
                       | j == 0 -> writeArray f (i, j) 1
                       | otherwise -> do part1 <- readArray f (i, j-1)
                                         part2 <- sum <$> sequence [readArray f (i - k*2^j, j - 1) | k <- [1 .. (min 9 (i `div` (2^j)))]]
                                         writeArray f (i, j) (part1 + part2)
        cur <- lift $ readArray f (i, maxBinaryIndex i)
        lift $ writeArray fSum i (prevSum + cur)

      ff <- unsafeFreeze f
      ffSum <- unsafeFreeze fSum
      return (ff, ffSum)

    -- special treatment for 0
    maxBinaryIndex :: Int -> Int
    maxBinaryIndex 0 = 0
    maxBinaryIndex x = floor . logBase 2 . fromIntegral $ x
    -- m, n determines the maximum index of the cache array: (0, 0) to (m, n)
    -- TODO: properly determine m
    m = 300000 :: Int
    n = maxBinaryIndex m

    -- solve xth = traceShow fSum 0
    solve xth = solve' initialI (maxBinaryIndex initialI) (xth - previousSum)
      where
        initialI = findInIncreasingArrayLike (>= xth) 0 m fSum (!)
        previousSum = if initialI == 0 then 0 else fSum ! (initialI - 1)

        -- uses linear search (findIndex), seems that binary search is slower in this case
        findWhichJNthFallsInto nth i = fromIntegral $ fromJust $ findIndex (>= nth) [f ! (i, j) | j <- [0..]]

        solve' :: Int -> Int -> Int -> Int
        solve' i j nth | 0 <= i && i <= 9 && j <= 0 = i
                       | i == 0 = 0
                       | (0 > i || i > 9) && j <= 0 = error "Impossible"
                       | otherwise = subK*10^subJ + solve' (i - subK*2^subJ) (subJ - 1) (subNth - subPrevSum)
          where
            subJ = findWhichJNthFallsInto nth i
            subNth = if subJ == 0 then 1 else nth - f ! (i, subJ - 1)
            (subPrevSum, subKMinus1) = if subJ == 0 then (0, 0) else findPreviousSumWithIndex (>= subNth) [f ! (i - k*2^subJ, subJ - 1) | k <- [1..]]
            subK = subKMinus1 + 1

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  qTemp <- getLine
  let q = read qTemp :: Int

  queries <- replicateM q $ do
    xTemp <- B.getLine
    let x = fst $ fromJust $ B.readInt xTemp :: Int
    return x

  let result = decibinaryNumbers queries

  for_ result (hPrint fptr)

  hFlush fptr
  hClose fptr
