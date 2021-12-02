import System.IO
import Data.Text as T (pack, unpack, splitOn)

solution :: [Int] -> Int
solution (x:y:z:a:as)
  | secondThree > firstThree = 1 + next
  | otherwise                = 0 + next
  where
    firstThree  = x + y + z
    secondThree = y + z + a
    next        = solution $ y:z:a:as
solution _ = 0

main :: IO ()
main = do
  input <- readFile "input.txt"
  print . solution $ (\n -> read (unpack n) :: Int) <$> T.splitOn (pack "\n") (pack input)
