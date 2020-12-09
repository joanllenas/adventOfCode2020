module Day8
  ( day8,
    Instruction (..),
    Visited (..),
    instructionParser,
  )
where

import qualified Control.Lens as Lens
import Data.Either (rights)
import qualified Text.Parsec as P
import Text.Parsec.Char
import qualified Text.Parsec.Error (ParseError (..))
import Text.Parsec.String (Parser)
import qualified Utils as U

newtype Visited = Vis Bool deriving (Show, Eq)

data Instruction
  = Nop Int Visited
  | Acc Int Visited
  | Jmp Int Visited
  deriving (Show, Eq)

data Program = Prg
  { instructions :: [Instruction],
    position :: Int,
    value :: Int
  }

instructionParser :: Parser Instruction
instructionParser = do
  op <- P.choice [P.string "nop", P.string "acc", P.string "jmp"] <* P.char ' '
  n <- U.int <* P.eof
  return $ case op of
    "nop" -> Nop n (Vis False)
    "acc" -> Acc n (Vis False)
    "jmp" -> Jmp n (Vis False)

----------------
-- Part1
----------------

createProgram :: [Instruction] -> Program
createProgram inst = Prg inst 0 0

runProgram1 :: Program -> Int
runProgram1 (Prg is pos acc) = case is Lens.^? Lens.element pos of
  Nothing -> acc -- tried to reach `length is + 1`
  Just (Nop val (Vis v)) -> if v then acc else runProgram1 (Prg (setInstructionAt pos (Nop val (Vis True)) is) (pos + 1) acc)
  Just (Acc val (Vis v)) -> if v then acc else runProgram1 (Prg (setInstructionAt pos (Acc val (Vis True)) is) (pos + 1) (acc + val))
  Just (Jmp val (Vis v)) -> if v then acc else runProgram1 (Prg (setInstructionAt pos (Jmp val (Vis True)) is) (pos + val) acc)
  where
    setInstructionAt :: Int -> Instruction -> [Instruction] -> [Instruction]
    setInstructionAt pos instr is = is Lens.& Lens.element pos Lens..~ instr

part1 :: String -> String
part1 =
  (++) "Part 1 result is: "
    . show
    . runProgram1
    . createProgram
    . rights
    . map (U.parseString instructionParser)
    . lines

----------------
-- Part2
----------------

runProgram2 :: Program -> Int
runProgram2 (Prg is pos acc) = -1

part2 :: String -> String
part2 input = "Part 2 result is: TODO"

----------------
-- Main
----------------

day8 :: IO ()
day8 = U.handleResultWith 8 part1 part2
