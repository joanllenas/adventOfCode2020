module Utils (getInputData) where

import qualified Control.Exception as Exception
import qualified System.Directory as Dir

---------------------
-- Get Input Data
---------------------

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
