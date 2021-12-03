import Data.Text as T  (pack, unpack, splitOn)
import Data.Function   (on)
import Data.List       (transpose, sort, group, sortBy, filter, isPrefixOf)
import Data.Char       (digitToInt)
import Numeric         (readInt)

readBin :: String -> Int
readBin = fst . head . readInt 2 (`elem` "01") digitToInt

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

solution :: [String] -> Int
solution xs = ((*) `on` (readBin . head)) (mostCommon 0 xs) (lessCommon 0 xs)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print . solution . map T.unpack . (T.splitOn `on` T.pack) "\n" $ input
