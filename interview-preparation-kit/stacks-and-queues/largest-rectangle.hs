module Main where

import Prelude hiding ((!), (//))
import Data.Int
import Data.List
import Data.Vector.Unboxed as V
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import System.Environment
import System.IO
import Control.Monad.Trans.Reader

--
-- Complete the 'largestRectangle' function below.
--
-- The function is expected to return a LONG_INTEGER.
-- The function accepts INTEGER_ARRAY h as parameter.
--

type Height = Int64
type Area = Int64
type Idx = Int

data IterParam = IterParam { heights :: Vector Height
                           , indexStepper :: Idx -> Idx
                           , stopPredicate :: Idx -> Bool
                           }
type Iter a = Reader IterParam a

go :: [Area] -> Idx -> Idx -> [(Height, Idx)] -> Iter [Area]
go areas i prevIdx incHeightIdxStack = do
  heights <- asks heights
  indexStepper <- asks indexStepper
  stopPredicate <- asks stopPredicate
  let h = heights ! i
  if stopPredicate i then return areas
    else case compare h (heights ! prevIdx) of
           LT ->
             let
               (eqHighers, lowers) = Data.List.span (\(height, index) -> height >= h) incHeightIdxStack
               (_, eqHigherIdx) = Data.List.last eqHighers
               additionalArea = fromIntegral (abs (i - eqHigherIdx)) * h
             in
               go (additionalArea:areas) (indexStepper i) i ((h, eqHigherIdx):lowers)
           EQ ->
             let
               (eqHighers, lowers) = Data.List.span (\(height, index) -> height >= h) incHeightIdxStack
               (_, eqHigherIdx) = Data.List.last eqHighers
               additionalArea = fromIntegral (abs (i - eqHigherIdx)) * h
             in
               go (additionalArea:areas) (indexStepper i) i incHeightIdxStack
           GT -> go (0:areas) (indexStepper i) i ((h, i):incHeightIdxStack)

largestRectangle :: [Height] -> Area
largestRectangle h =
  let heights = V.fromList h :: Vector Height
      firstHeight = heights ! 0
      len = V.length heights
      areasFirstPass = Data.List.reverse $ runReader (go [0] 1 0 [(firstHeight, 0)])
        (IterParam { heights = heights, indexStepper = (+ 1), stopPredicate = (== len) })
      areasSecondPass = runReader (go [0] (len - 2) (len - 1) [(V.last heights, len - 1)])
           (IterParam { heights = heights, indexStepper = \v -> v - 1, stopPredicate = (== -1) })
   in Data.List.maximum $ Data.List.zipWith3 (\a b c -> a + b + c) h areasFirstPass areasSecondPass

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  n <- B.getLine

  hTemp <- B.getLine

  let h = Data.List.map (fromIntegral . fst . fromJust . B.readInt) . B.words $ hTemp

  let result = largestRectangle h

  hPrint fptr result

  hFlush fptr
  hClose fptr
