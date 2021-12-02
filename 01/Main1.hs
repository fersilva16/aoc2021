import System.IO
import Data.Text as T (pack, unpack, splitOn)

solution :: [Int] -> Int
solution (x:y:ys)
  | y > x     = 1 + next
  | otherwise = 0 + next
  where next = solution $ y:ys
solution _ = 0

main :: IO ()
main = do
  input <- readFile "input.txt"
  print . solution $ (\n -> read (unpack n) :: Int) <$> T.splitOn (pack "\n") (pack input)
