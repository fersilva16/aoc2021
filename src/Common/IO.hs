module Common.IO (solution, solution2) where

import Text.Printf

type Parser    a   = String -> a
type Parser2   a b = String -> (a, b)
type Solution  a   = a -> Int
type Solution2 a b = a -> b -> Int

getInput :: String -> IO String
getInput = readFile . ("input/" ++) . (++ ".txt")

printSolution :: String -> Int -> Int -> IO ()
printSolution day a b = do
  printf "%sa = %d\n" day a
  printf "%sb = %d\n" day b

solution :: String -> Parser a -> Solution a -> Solution a -> IO ()
solution day parser sola solb = do
  input <- getInput day

  let input' = parser input

  printSolution day (sola input') (solb input')

solution2 :: String -> Parser2 a b -> Solution2 a b -> Solution2 a b -> IO ()
solution2 day parser sola solb = do
  input <- getInput day

  let (input', input'') = parser input

  printSolution day (sola input' input'') (solb input' input'')

