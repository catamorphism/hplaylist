module Flags where

import Control.Exception.Base
import Control.Monad
import System.Console.GetOpt
import System.Directory

tempDir :: [Flag] -> IO FilePath
tempDir fs | Just p <- getTempDir fs = do
  parentExists <- doesDirectoryExist p
--  putStrLn ("mkTempFileName: " ++ show (parent, parentExists))
  unless parentExists
    (throwIO (userError ("directory doesn't seem to exist: " ++ p)))
  return p
tempDir _ = getCurrentDirectory

getTempDir :: [Flag] -> Maybe FilePath
getTempDir = foldr (\ f rest ->
  case f of
    (TempDir p) -> Just p
    _           -> rest) Nothing

options :: [OptDescr Flag]
options = [Option ['d'] ["dry-run"] (NoArg DryRun)
  "Don't copy any files to the music player, just generate a script to do so",
           Option ['q'] ["quiet"] (NoArg Quiet)
  "Don't print too much output",
           Option [] ["temp-dir"] (ReqArg TempDir "DIR")
  "Directory in which to store temporary script files (default: current working directory)",
           Option ['k'] ["keep-temp-files"] (NoArg KeepTempFiles)
  "Don't delete any intermediate files (use this option for debugging)"]

data Flag = DryRun | Quiet | TempDir FilePath | KeepTempFiles
  deriving Eq

isDryRun :: [Flag] -> Bool
isDryRun = (DryRun `elem`)

isQuiet :: [Flag] -> Bool
isQuiet = (Quiet `elem`)

keepTmpFiles :: [Flag] -> Bool
keepTmpFiles = (KeepTempFiles `elem`)