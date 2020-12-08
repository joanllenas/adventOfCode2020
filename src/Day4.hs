module Day4
  ( day4,
    KeyValue (Kv),
    keyValueParser,
    validatePasswordRequiredKeys,
    hexParser,
    rightKeyValues,
    validateKey,
    validatePassword,
    validatePasswordKeys,
    Password,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Either (rights)
import Data.Either.Combinators (mapLeft)
import Data.List.Split (splitWhen)
import qualified Data.Map as Map
import Data.Monoid
import Debug.Trace (trace)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.Read
import qualified Utils as U
import qualified Validation as Val

data KeyValue = Kv String String deriving (Show, Eq)

instance Semigroup KeyValue where
  Kv k v <> Kv _ v' = Kv k (v <> v')

instance Monoid KeyValue where
  mempty = Kv "" ""

type Password = [KeyValue]

keyValueParser :: Parser KeyValue
keyValueParser = do
  key <- P.count 3 P.letter
  P.char ':'
  val <- P.many1 P.anyChar <* P.eof
  return $ Kv key val

rightKeyValues :: [String] -> Password
rightKeyValues passGrp = rights $ map (U.parseString keyValueParser) passGrp

validatePasswordRequiredKeys :: Password -> Either String Password
validatePasswordRequiredKeys kvs =
  let keys :: String
      keys = foldr (\(Kv k _) acc -> acc ++ k) "" kvs
      hasCid = elem "cid" $ map (\(Kv k _) -> k) kvs
      -- 3*8 is the length of all concatenated key Strings
      hasReqKeys = hasCid && length keys == (3 * 8) || not hasCid && length keys == (3 * 7)
   in if hasReqKeys
        then Right kvs
        else Left "Password doesn't have all required keys."

---------------------------------
-- Part 1
---------------------------------

part1 :: String -> String
part1 s =
  (++) "Part 1 result is:" . show $
    length $
      rights $
        map (validatePasswordRequiredKeys . rightKeyValues . concat) $
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
  b <- P.count 6 (P.choice [P.digit, abcdefParser]) <* P.eof
  return $ a : b

heightParser :: Parser (String, String)
heightParser = do
  n <- P.many1 P.digit
  l <- P.choice [P.string "in", P.string "cm"] <* P.eof
  return (n, l)

validateNumBounds :: Int -> Int -> KeyValue -> Val.Validation String KeyValue
validateNumBounds min max kv@(Kv _ n) = case readMaybe n :: Maybe Int of
  Just num ->
    if num >= min && num <= max
      then Val.Success kv
      else Val.Failure $ "Number " ++ n ++ " out of bounds."
  Nothing -> Val.Failure $ "Invalid number " ++ n ++ "."

validateHex :: KeyValue -> Val.Validation String KeyValue
validateHex kv@(Kv _ s) = case U.parseString hexParser s of
  Right _ -> Val.Success kv
  Left _ -> Val.Failure $ "Could not parse hex: " ++ s ++ "."

validateColor :: KeyValue -> Val.Validation String KeyValue
validateColor kv@(Kv _ s) =
  if s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    then Val.Success kv
    else Val.Failure $ "Invalid color: " ++ s ++ "."

validatePid :: KeyValue -> Val.Validation String KeyValue
validatePid kv@(Kv _ s) = case U.parseString pidParser s of
  Right _ -> Val.Success kv
  Left _ -> Val.Failure $ "Invalid pid: " ++ s ++ "."
  where
    pidParser :: Parser String
    pidParser = P.count 9 P.digit <* P.eof

validateHeight :: KeyValue -> Val.Validation String KeyValue
validateHeight kv@(Kv _ s) = case U.parseString heightParser s of
  Right (n, "cm") -> kv <$ validateNumBounds 150 193 (Kv "" n)
  Right (n, "in") -> kv <$ validateNumBounds 59 76 (Kv "" n)
  Left _ -> Val.Failure $ "Could not parse height: " ++ s ++ "."

validations :: Map.Map String (KeyValue -> Val.Validation String KeyValue)
validations =
  Map.fromList
    [ ("byr", validateNumBounds 1920 2002),
      ("iyr", validateNumBounds 2010 2020),
      ("eyr", validateNumBounds 2020 2030),
      ("hgt", validateHeight),
      ("hcl", validateHex),
      ("ecl", validateColor),
      ("pid", validatePid),
      ("cid", \(Kv k v) -> Val.Success (Kv k v))
    ]

validateKey :: KeyValue -> Val.Validation String KeyValue
validateKey (Kv k v) = case Map.lookup k validations of
  Just fn -> fn (Kv k v)
  Nothing -> Val.Failure $ "Key " ++ k ++ "not found."

validatePassword :: Password -> Val.Validation String Password
validatePassword p = case foldMap validateKey p of
  Val.Success _ -> Val.Success p
  Val.Failure e -> Val.Failure $ "Invalid password: " ++ e

validatePasswordKeys :: Either String Password -> Either String Password
validatePasswordKeys password =
  password >>= Val.validationToEither . validatePassword

part2 :: String -> String
part2 s =
  (++) "Part 2 result is:" . show $
    length $
      rights $
        map (validatePasswordKeys . validatePasswordRequiredKeys . rightKeyValues . concat) $
          splitWhen (== []) $ -- [ [[String]] ]
            map words $ -- [ [String] ]
              lines s -- [String]

-- Main

day4 :: IO ()
day4 = U.handleResultWith 4 part1 part2
