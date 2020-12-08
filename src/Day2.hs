module Day2 (day2, parsePasswordDef, PasswordDef (MkPasswordDef)) where

import Data.Char (digitToInt)
import Data.Either (rights)
import qualified Data.List as List
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import qualified Utils as U

----------------
-- Data Model
----------------

data PasswordDef = MkPasswordDef
  { n1 :: Int,
    n2 :: Int,
    letter :: Char,
    password :: String
  }
  deriving (Show, Eq)

----------------
-- Parsing
----------------

chr :: Parser Char
chr = P.letter

lastWord :: Parser String
lastWord = P.many1 P.letter <* P.eof

positiveNatural =
  List.foldl' (\a i -> a * 10 + digitToInt i) 0 <$> P.many1 P.digit

passwordDefP :: Parser PasswordDef
passwordDefP = do
  n1 <- positiveNatural
  P.char '-'
  n2 <- positiveNatural
  P.char ' '
  l <- chr
  P.char ':'
  P.char ' '
  MkPasswordDef n1 n2 l <$> lastWord

parsePasswordDef :: String -> Either P.ParseError PasswordDef
parsePasswordDef = U.parseString passwordDefP

parsePasswordDefs :: [String] -> [Either P.ParseError PasswordDef]
parsePasswordDefs = map parsePasswordDef

-- Count Valid Passwords and give back result

calculateResult :: ([String] -> [Either () PasswordDef]) -> [String] -> String
calculateResult validatePasswordFn pwLines =
  let nValidPasswords :: Int
      nValidPasswords = length $ rights . validatePasswordFn $ pwLines
   in "There are " ++ show nValidPasswords ++ " valid passwords." --- ++ show (rights . validatePasswordFn $ pwLines)

----------------
-- Part 1
----------------

validatePart1PasswordDefs :: [String] -> [Either () PasswordDef]
validatePart1PasswordDefs xs =
  map validatePw $ parsePasswordDefs xs
  where
    occurences :: Char -> String -> Int
    occurences c str = length $ filter (== c) str
    validatePw :: Either P.ParseError PasswordDef -> Either () PasswordDef
    validatePw (Left _) = Left ()
    validatePw (Right pDef) =
      let nOccurences = occurences (letter pDef) (password pDef)
       in if nOccurences <= n2 pDef && nOccurences >= n1 pDef
            then Right pDef
            else Left ()

part1 :: String -> String
part1 inputData =
  let result = calculateResult validatePart1PasswordDefs $ lines inputData
   in "Part 1 result is: " ++ result

----------------
-- Part 2
----------------

validatePart2PasswordDefs :: [String] -> [Either () PasswordDef]
validatePart2PasswordDefs xs =
  map validatePw $ parsePasswordDefs xs
  where
    chIndices :: Char -> String -> [Int]
    chIndices ch str = List.elemIndices ch str

    validatePw :: Either P.ParseError PasswordDef -> Either () PasswordDef
    validatePw (Left _) = Left ()
    validatePw (Right pwDef) =
      let indices = map (+ 1) (chIndices (letter pwDef) (password pwDef))
          matches = filter (\idx -> idx == n1 pwDef || idx == n2 pwDef) indices
       in if length matches == 1
            then Right pwDef
            else Left ()

part2 :: String -> String
part2 inputData =
  let result = calculateResult validatePart2PasswordDefs $ lines inputData
   in "Part 2 result is: " ++ result

----------------
-- Main
----------------

day2 :: IO ()
day2 = U.handleResultWith 2 part1 part2
