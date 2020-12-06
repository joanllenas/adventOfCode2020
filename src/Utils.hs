module Utils (handleResultWith, parse, hasSuffix) where

import qualified Control.Exception as Exception
import qualified System.Directory as Dir
import qualified Text.Parsec as P
import qualified Text.Parsec.String as Ps

------------------------------------------
-- Parsing
------------------------------------------

parse :: Ps.Parser a -> String -> Either P.ParseError a
parse p = P.parse p ""

------------------------------------------
-- Get Input Data
------------------------------------------

readFileE :: String -> IO (Either Exception.IOException String)
readFileE f = Exception.try $ readFile f

getDataFilePath :: String -> IO String
getDataFilePath file = do
  cwd <- Dir.getCurrentDirectory
  return $ cwd ++ "/data/" ++ file

getInputData :: String -> IO (Either Exception.IOException String)
getInputData file = do
  filePath <- getDataFilePath file
  readFileE filePath

------------------------------------------
-- Result handling boilerplate
------------------------------------------

type Input = String

type PartFn = Input -> String

handleResultWith :: Int -> PartFn -> PartFn -> IO ()
handleResultWith day pt1 pt2 = do
  let d = show day
  putStrLn $ "-- Day " ++ d ++ " --"
  inputData <- getInputData $ "day-" ++ d ++ ".txt"
  case inputData of
    Left err -> putStrLn $ "Day " ++ d ++ ": Error reading input file: " ++ show err
    Right input -> do
      putStrLn $ pt1 input
      putStrLn $ pt2 input

------------------------------------------
-- Misc Utils
------------------------------------------

hasSuffix :: String -> String -> Bool
hasSuffix suffix s =
  let pos = (length s) - (length suffix)
   in drop pos s == suffix
