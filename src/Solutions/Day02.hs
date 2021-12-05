module Solutions.Day02 where

import Common.IO

data Command
  = Forward Int
  | Down    Int
  | Up      Int

parseCommand :: [String] -> Command
parseCommand (c:s:_)
  = case c of
    "forward" -> Forward steps
    "down"    -> Down steps
    "up"      -> Up steps
    otherwise -> error "Unreachable"
  where
    steps = read s

evalCommandsA :: (Int, Int) -> [Command] -> (Int, Int)
evalCommandsA (h, d) (c:cs) = evalCommandsA currentPosition cs
  where
    currentPosition = case c of
      (Forward steps) -> (h + steps, d)
      (Down steps)    -> (h, d + steps)
      (Up steps)      -> (h, d - steps)
evalCommandsA p _ = p

day02a :: [Command] -> Int
day02a cs = totalHorizontal * totalDepth
  where (totalHorizontal, totalDepth) = evalCommandsA (0, 0) cs


evalCommandsB :: (Int, Int, Int) -> [Command] -> (Int, Int, Int)
evalCommandsB (h, d, t) (c:cs) = evalCommandsB currentPosition cs
  where
    currentPosition = case c of
      (Forward steps) -> (h + steps, d, t + d * steps)
      (Down steps)    -> (h, d + steps, t)
      (Up steps)      -> (h, d - steps, t)
evalCommandsB p _ = p

day02b :: [Command] -> Int
day02b cs = totalHorizontal * totalDepth
  where (totalHorizontal, _, totalDepth) = evalCommandsB (0, 0, 0) cs



day02 :: IO ()
day02 = solution "02" (map (parseCommand . words) . lines) day02a day02b
