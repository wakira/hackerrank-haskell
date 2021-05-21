module Main where

-- import Data.Vector (Vector, generate, (!), fromList)
import Control.Monad.ST.Strict (runST)
import Control.Monad (forM_)
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as B
import System.Environment
import System.IO

--
-- Complete the 'commonChild' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. STRING s1
--  2. STRING s2
--

-- this lazy-evaluation version seems to run out of stack memory
-- lcs s1 s2 = f ! (iLen * jLen - 1)
--     where
--         iLen = B.length s1 + 1
--         jLen = B.length s2 + 1

--         mkIdx i j = i * jLen + j
--         getI idx = idx `div` jLen
--         getJ idx = idx `mod` jLen

--         f = generate (iLen * jLen) fGenerator

--         fGenerator idx | getI idx == 0 = 0
--         fGenerator idx | getJ idx == 0 = 0
--         fGenerator idx = if s1 `B.index` (i - 1) == s2 `B.index` (j - 1) then (f ! mkIdx (i-1) (j-1)) + 1
--                          else max (f ! mkIdx (i-1) j) (f ! mkIdx i (j-1))
--             where
--                 i = getI idx
--                 j = getJ idx

lcs :: B.ByteString -> B.ByteString -> Int
lcs s1 s2 = runST $ do
    f <- VM.new jLen -- prev
    g <- VM.new jLen -- current
    forM_ [0..jLen - 1] $ \j -> VM.write f j 0

    forM_ [1..iLen - 1] $ \i ->
        forM_ [1..jLen - 1] $ \j ->
            if odd i then VM.write g 0 0 >> go f g i j
            else VM.write f 0 0 >> go g f i j

    if odd (iLen - 1) then VM.read g (jLen - 1) else VM.read f (jLen - 1)

    where
        go f g i j =
            if s1 `B.index` (i-1) == s2 `B.index` (j-1) then
                VM.read f (j-1) >>= VM.write g j . (+1)
            else
                max <$> VM.read f j <*> VM.read g (j-1) >>= VM.write g j

        iLen = B.length s1 + 1
        jLen = B.length s2 + 1

commonChild = lcs

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s1 <- B.getLine

    s2 <- B.getLine

    let result = commonChild s1 s2

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
