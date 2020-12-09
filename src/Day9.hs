module Day9 (day9) where

import Debug.Trace (trace)
import qualified Utils as U

preamble :: Int
preamble = 25

data Program = Prg
  { list :: [Int],
    cursor :: Int
  }
  deriving (Show, Eq)

----------------
-- Part 1
----------------

createProgram :: [Int] -> Program
createProgram list = Prg list preamble

crackXmas :: Program -> Program
crackXmas (Prg list cursor) =
  let l = drop (cursor - preamble) $ take cursor list
      n = list !! cursor
   in -- nn = trace (show l ++ " cursor:" ++ show cursor ++ " num:" ++ show n) n
      case [(x, y) | x <- l, y <- l, x + y == n, x /= y] of
        [] -> Prg list cursor
        _ -> crackXmas (Prg list (cursor + 1))

part1 :: String -> String
part1 =
  show
    . (\(Prg list cursor) -> list !! cursor)
    . crackXmas
    . createProgram
    . map (\l -> read l :: Int)
    . lines

----------------
-- Part 2
----------------

part2 :: String -> String
part2 input = "Part 2 result is: TODO"

----------------
-- Main
----------------

day9 :: IO ()
day9 = U.handleResultWith 9 part1 part2
