module Solutions.Day03 where

import Data.Function   (on)
import Data.List       (transpose, sort, group, sortBy, filter)
import Data.Char       (digitToInt)
import Numeric         (readInt)
import Common.IO

readBin :: String -> Int
readBin = fst . head . readInt 2 (`elem` "01") digitToInt

invertBin :: String -> String
invertBin = map (\x -> if x == '0' then '1' else '0')


day03a :: [String] -> Int
day03a xs = ((*) `on` readBin) gr (invertBin gr)
  where gr = map (head . last . sortBy (compare `on` length) . group . sort) . transpose $ xs


mostCommon :: Int -> [String] -> [String]
mostCommon _ [x] = [x]
mostCommon s xs = mostCommon (s + 1) . filter ((==) p . (flip (!!)) s) $ xs
  where
    p = if ((==) `on` length) a b then '1' else head b
    [a, b] = sortBy (compare `on` length) . group . sort . (flip (!!)) s . transpose $ xs

lessCommon :: Int -> [String] -> [String]
lessCommon _ [x] = [x]
lessCommon s xs = lessCommon (s + 1) . filter ((==) p . (flip (!!)) s) $ xs
  where
    p = if ((==) `on` length) a b then '0' else head a
    [a, b] = sortBy (compare `on` length) . group . sort . (flip (!!)) s . transpose $ xs

day03b :: [String] -> Int
day03b xs = ((*) `on` (readBin . head)) (mostCommon 0 xs) (lessCommon 0 xs)


day03 :: IO ()
day03 = solution "03" lines day03a day03b
