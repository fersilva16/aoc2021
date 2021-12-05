module Solutions.Day01 where

import Common.IO

day01a :: [Int] -> Int
day01a (x:y:ys)
  | y > x     = 1 + n
  | otherwise = 0 + n
  where n = day01a $ y:ys
day01a _ = 0

day01b :: [Int] -> Int
day01b (x:y:z:a:as)
  | (y + z + a) > (x + y + z) = 1 + n
  | otherwise                 = 0 + n
  where n = day01b $ y:z:a:as
day01b _ = 0


day01 :: IO ()
day01 = solution "01" (map read . lines) day01a day01b
