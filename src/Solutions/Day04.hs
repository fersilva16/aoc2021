{-# LANGUAGE TupleSections #-}

module Solutions.Day04 where

import Common.IO
import Common.Trim
import Data.List.Split (splitOn)
import Data.List (find, transpose)
import Data.Maybe (isJust, fromJust)

bingo :: [[(Int, Bool)]] -> Bool
bingo b = f b || (f . transpose $ b)
  where f = any (all snd)

day04a :: [Int] -> [[[(Int, Bool)]]] -> Int
day04a (n:ns) bs
  | isJust res = (* n) . sum . map (sum . map fst . filter (not . snd)) . fromJust $ res
  | otherwise  = day04a ns nbs
  where
    res = find bingo nbs
    nbs = map (map (map (\(x, y) -> (x, y || x == n)))) bs
day04a _ _ = error "Unreachable"

day04b :: [Int] -> [[[(Int, Bool)]]] -> Int
day04b (n:ns) bs
  | length nbs == 1 = day04a (n:ns) nbs
  | otherwise       = day04b ns nbs
  where
    nbs = filter (not . bingo) . map (map (map (\(x, y) -> (x, y || x == n)))) $ bs
day04b _ _ = error "Unreachable"

parser :: String -> ([Int], [[[(Int, Bool)]]])
parser input = (ns, bs)
  where
    ns = map read . splitOn "," $ uns
    bs = map (map (map ((,False) . read) . words . trim) . lines) ubs
    (uns:ubs) = splitOn "\n\n" input

day04 :: IO ()
day04 = solution2 "04" parser day04a day04b
