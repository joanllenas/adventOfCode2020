module Day4 (day4, KeyValue (Kv), keyValueParser, passwordHasRequiredKeys, hexParser) where

import Data.Either (rights)
import Data.Either.Combinators (mapLeft)
import Data.List.Split (splitWhen)
import Debug.Trace (trace)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Read
-- (Validation (..))
import qualified Utils as U
import qualified Validation as Val

data KeyValue = Kv String String deriving (Show, Eq)

keyValueParser :: Parser KeyValue
keyValueParser = do
  key <- P.count 3 P.letter
  P.char ':'
  val <- P.many1 P.anyChar <* P.eof
  return $ Kv key val

rightKeyValues :: [String] -> [KeyValue]
rightKeyValues passGrp = rights $ map (U.parse keyValueParser) passGrp

passwordHasRequiredKeys :: [KeyValue] -> Bool
passwordHasRequiredKeys kvs =
  let keys :: String
      keys = foldr (\(Kv k _) acc -> acc ++ k) "" kvs
      hasCid = elem "cid" $ map (\(Kv k _) -> k) kvs
   in -- 3*8 is the length of all concatenated key Strings
      hasCid && length keys == (3 * 8) || not hasCid && length keys == (3 * 7)

---------------------------------
-- Part 1
---------------------------------

part1 :: String -> String
part1 s =
  show $
    length $
      filter (== True) $
        map (passwordHasRequiredKeys . rightKeyValues . concat) $
          splitWhen (== []) $ -- [ [[String]] ]
            map words $ -- [ [String] ]
              lines s -- [String]

---------------------------------
-- Part 2
---------------------------------

abcdefParser :: Parser Char
abcdefParser = P.choice [P.char 'a', P.char 'b', P.char 'c', P.char 'd', P.char 'e', P.char 'f']

hexParser :: Parser String
hexParser = do
  a <- P.char '#'
  b <- P.many1 (P.choice [P.digit, abcdefParser]) <* P.eof
  return $ a : b

validateNumBounds :: Int -> Int -> String -> Either String String
validateNumBounds up down s = case readMaybe s :: Maybe Int of
  Just yr ->
    if yr >= 1920 && yr <= 2002
      then Right s
      else Left $ "Number " ++ s ++ " out of bounds"
  Nothing -> Left $ "Invalid year " ++ s

validateSuffix :: String -> String -> Either String String
validateSuffix suffix s =
  if U.hasSuffix suffix s
    then Right s
    else Left "Suffix not found"

validateHex :: String -> Either String String
validateHex = mapLeft show . U.parse hexParser

part2 :: String -> String
part2 s = "TODO"

-- Main

day4 :: IO ()
day4 = U.handleResultWith 4 part1 part2
