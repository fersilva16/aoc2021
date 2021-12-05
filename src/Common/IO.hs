module Common.IO where

import Text.Printf

type Parser   a = String -> a
type Solution a = a -> Int

solution :: String -> Parser a -> Solution a -> Solution a -> IO ()
solution day parser sola solb = do
  input <- readFile $ "input/" ++ day ++ ".txt"

  let input' = parser input

  printf "%sa = %d\n" day (sola input')
  printf "%sb = %d\n" day (solb input')

