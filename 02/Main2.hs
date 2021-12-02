import System.IO
import Data.Text as T (pack, unpack, splitOn, Text)

data Command
  = Forward Int
  | Down    Int
  | Up      Int

type Position = (Int, Int, Int)

evalCommands :: Position -> [Command] -> Position
evalCommands (h, d, t) (c:cs) = evalCommands currentPosition cs
  where
    currentPosition = case c of
      (Forward steps) -> (h + steps, d, t + d * steps)
      (Down steps)    -> (h, d + steps, t)
      (Up steps)      -> (h, d - steps, t)
evalCommands position _ = position

solution :: [Command] -> Int
solution cs = totalHorizontal * totalDepth
  where (totalHorizontal, _, totalDepth) = evalCommands (0, 0, 0) cs


parseCommand :: Text -> Command
parseCommand c
  = case command of
    "forward" -> Forward steps
    "down"    -> Down steps
    "up"      -> Up steps
    otherwise -> error "Unreachable"
  where
    [ucommand, usteps] = T.splitOn (pack " ") c
    steps = read (unpack usteps) :: Int
    command = unpack ucommand


main :: IO ()
main = do
  input <- readFile "input.txt"
  print . solution $ parseCommand <$> T.splitOn (pack "\n") (pack input)
