module Main where

import Data.List ( words )
import Data.Foldable (foldl')
import Data.Text ( pack, stripEnd, stripStart, unpack )
import Control.Monad.Reader
import Data.IORef
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, openFile, hPrint, IOMode(WriteMode) )

--
-- Complete the 'minimumPasses' function below.
--
-- The function is expected to return a LONG_INTEGER.
-- The function accepts following parameters:
--  1. LONG_INTEGER m
--  2. LONG_INTEGER w
--  3. LONG_INTEGER p
--  4. LONG_INTEGER n
--

mwAfterOptimalBuy curM curW amount =
  let diff = curW - curM
      amountToEqualize = min amount diff
      amountAfterEqualize = amount - amountToEqualize
      amountAfterEqualizeGoToM = amountAfterEqualize `div` 2
      amountAfterEqualizeGoToW = amountAfterEqualize - amountAfterEqualizeGoToM
      newM = curM + amountToEqualize + amountAfterEqualizeGoToM
      newW = curW + amountAfterEqualizeGoToW
   in (newM, newW)

investOrSpend :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
investOrSpend m w p n candies minRndSpend rnd
  | m * w + candies >= n = min minRndSpend rnd
  | otherwise =
    -- skip rounds where you cannot invest
    let rndsTillNextPossibleInvest = (p - m*w - candies) `div` (m*w)
     in if rndsTillNextPossibleInvest > 0
        then investOrSpend m w p n (m*w*rndsTillNextPossibleInvest + candies) minRndSpend (rnd + rndsTillNextPossibleInvest)
        else
          let maxBuy = (m*w + candies) `div` p
              (afterInvestM, afterInvestW) = mwAfterOptimalBuy m w maxBuy
              candiesAfterInvest = (m*w + candies) `mod` p
              spend = min minRndSpend (rnd + ceiling (fromInteger (n - candies - m*w) / fromInteger (m*w)))
          in investOrSpend afterInvestM afterInvestW p n candiesAfterInvest spend (rnd + 1)

minimumPasses m w p n =
  -- Write your code here
  investOrSpend mwMin mwMax p n 0 (ceiling $ fromInteger n / fromInteger (m*w)) 1
  where mwMin = min m w
        mwMax = max m w


rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  firstMultipleInputTemp <- getLine
  let [m, w, p, n] = map read $ Data.List.words $ rstrip firstMultipleInputTemp

  let result = minimumPasses m w p n

  hPrint fptr result

  hFlush fptr
  hClose fptr
