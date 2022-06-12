module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Control.Monad
import Data.Foldable

data Query = Enq Int | Deq | Prt

go [] _ _ = []
go (Prt:xs) s1 s2@(v2:s2s) = v2 : go xs s1 s2
go (Prt:xs) s1 [] = head s2 : go xs [] s2 where
  s2 = reverse s1
go (Deq:xs) s1 [] = go xs [] $ tail $ reverse s1
go (Deq:xs) s1 (v2:s2s) = go xs s1 s2s
go (Enq v:xs) s1 s2 = go xs (v : s1) s2

main :: IO ()
main = do
  q <- fst . fromJust . B.readInt <$> B.getLine
  queries <- replicateM q $ do
    (q1, q2) <- fromJust . B.readInt <$> B.getLine
    case q1 of
      1 -> pure $ Enq $ fst . fromJust $ B.readInt $ B.drop 1 q2
      2 -> pure Deq
      3 -> pure Prt
      _ -> error "Invalid input"
  traverse_ print $ go queries [] []
