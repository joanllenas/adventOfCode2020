module Day9 (day9) where

import Data.List
import qualified Utils as U

preamble :: Int
preamble = 25

data Program = Prg
  { list :: [Int],
    cursor :: Int
  }
  deriving (Show, Eq)

createProgram :: [Int] -> Program
createProgram list = Prg list preamble

----------------
-- Part 1
----------------

crackXmas1 :: Program -> Program
crackXmas1 (Prg list cursor) =
  let l = drop (cursor - preamble) $ take cursor list
      n = list !! cursor
   in case [(x, y) | x <- l, y <- l, x + y == n, x /= y] of
        [] -> Prg list cursor
        _ -> crackXmas1 (Prg list (cursor + 1))

runProgram1 :: String -> Program
runProgram1 =
  crackXmas1
    . createProgram
    . map (\l -> read l :: Int)
    . lines

part1 :: String -> String
part1 =
  (++) "Part 1 result is: "
    . show
    . (\(Prg list cursor) -> list !! cursor)
    . runProgram1

----------------
-- Part 2
----------------

createWindows :: Int -> [Int] -> [[Int]]
createWindows size xs = transpose (take size (tails xs))

windowSliding :: [Int] -> Int -> Int -> Maybe [Int]
windowSliding list target size
  | size < length list `div` 2 =
    let ws = createWindows size list
     in case find (\xs -> sum xs == target) ws of
          Nothing -> windowSliding list target (size + 1)
          Just win -> Just win
  | otherwise = Nothing

crackXmas2 :: ([Int], Int) -> Int
crackXmas2 (list, target) = case windowSliding list target 2 of
  Just xs -> let xs' = sort xs in head xs' + last xs'
  Nothing -> 0

part2 :: String -> String
part2 =
  (++) "Part 2 result is: "
    . show
    . crackXmas2
    . (\(Prg list cursor) -> (list, list !! cursor))
    . runProgram1

----------------
-- Main
----------------

day9 :: IO ()
day9 = U.handleResultWith 9 part1 part2
