module Main where

import Data.List
import Data.Text
import Data.Map.Strict as M
import System.Environment
import System.IO

--
-- Complete the 'checkMagazine' function below.
--
-- The function accepts following parameters:
--  1. STRING_ARRAY magazine
--  2. STRING_ARRAY note
--

checkMagazine magazine note = do
    -- Write your code here
    let magazineMap = Prelude.foldr (\word m -> M.insertWith (+) word 1 m) M.empty magazine
    let noteMap = Prelude.foldr (\word m -> M.insertWith (+) word 1 m) M.empty note
    let result = Prelude.and $ Data.List.map
                     (\(word, freq) -> word `M.member` magazineMap && (magazineMap M.! word) >= freq)
                     (M.toAscList noteMap)
    if result then putStrLn "Yes" else putStrLn "No"

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let m = read (firstMultipleInput !! 0) :: Int

    let n = read (firstMultipleInput !! 1) :: Int

    magazineTemp <- getLine

    let magazine = Data.List.words $ rstrip magazineTemp

    noteTemp <- getLine

    let note = Data.List.words $ rstrip noteTemp

    checkMagazine magazine note
