module Solutions.Day06 where

import Common.IO
import Common.Trim
import Data.List.Split (splitOn)
import Control.Lens (element, (.~))
import Data.List (group, foldl)

day06s :: Int -> [Int] -> Int
day06s d ls
  | d > 0    = day06s (d - 1) . (++ t8) $ nls
  | otherwise = sum ls
  where
    nls = element 6 .~ (at6 + t6) $ rnls
    at6 = rnls !! 6
    (t8@[t6], rnls) = splitAt 1 ls

day06parser :: String -> [Int]
day06parser input = foldl (\acc x -> element x .~ ((acc !! x) + 1) $ acc) fi xs
  where
    xs = map (\n -> read n :: Int) . splitOn "," $ input
    fi = [0, 0, 0, 0, 0, 0, 0, 0, 0]

day06 :: IO ()
day06 = solution "06" day06parser (day06s 80) (day06s 256)
