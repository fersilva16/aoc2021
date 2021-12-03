import Data.Text as T (pack, unpack, splitOn)
import Data.Function  (on)
import Data.List      (transpose, sort, group, sortBy)
import Data.Char      (digitToInt)
import Numeric        (readInt)

readBin :: String -> Int
readBin = fst . head . readInt 2 (`elem` "01") digitToInt

invertBin :: String -> String
invertBin = map (\x -> if x == '0' then '1' else '0')

gammaRate :: [String] -> String
gammaRate = map (head . last . sortBy (compare `on` length) . group . sort) . transpose

solution :: [String] -> Int
solution xs = ((*) `on` readBin) gr (invertBin gr)
  where gr = gammaRate xs

main :: IO ()
main = do
  input <- readFile "input.txt"
  print . solution . map T.unpack . (T.splitOn `on` T.pack) "\n" $ input
