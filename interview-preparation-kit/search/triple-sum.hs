module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Semigroup (getSum, Sum(..))
import Data.Foldable (sum)
import Data.List ( map, sort )
import qualified Data.Map.Strict as Map
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, openFile, hPutStrLn, IOMode(WriteMode) )

-- Complete the triplets function below.
triplets a b c =
  go (uniqueIncreasing a) (uniqueIncreasing b) (uniqueIncreasing c) 0 0 0
  where
    uniqueIncreasing = sort . Set.toList . Set.fromList

    go _ [] _ _ _ count = count
    go aa (bb:bs) cc acount ccount count =
      let
        (al, ar) = span (<= bb) aa
        totLEInA = acount + length al
        (cl, cr) = span (<= bb) cc
        totLEInC = ccount + length cl
      in
        go ar bs cr totLEInA totLEInC (count + totLEInA*totLEInC)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
  line <- getLine
  rest <- readMultipleLinesAsStringArray (n - 1)
  return (line : rest)

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  lenaLenbLencTemp <- getLine
  let lenaLenbLenc = words lenaLenbLencTemp

  let lena = read (lenaLenbLenc !! 0) :: Int

  let lenb = read (lenaLenbLenc !! 1) :: Int

  let lenc = read (lenaLenbLenc !! 2) :: Int

  arraTemp <- B.getLine

  let arra = Data.List.map (fst . fromJust . B.readInt) . B.words $ arraTemp

  arrbTemp <- B.getLine

  let arrb = Data.List.map (fst . fromJust . B.readInt) . B.words $ arrbTemp

  arrcTemp <- B.getLine

  let arrc = Data.List.map (fst . fromJust . B.readInt) . B.words $ arrcTemp

  let ans = triplets arra arrb arrc

  hPutStrLn fptr $ show ans

  hFlush fptr
  hClose fptr
