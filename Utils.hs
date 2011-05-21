module Utils(escape, mapFile, rsyncCommand, uniqify,
 mkSysTempFileName, removeAFile, deleteFiles, removeFilesOnExit) where

import Flags

import Control.Monad
import Data.Char
import Data.List
import System.IO
import System.Directory
import System.FilePath

escape :: String -> String
escape s = go s
  where go (c:rest) | isSpecial c = "\\"++[c]++(go rest)
        go (c:rest) | ord c >= 128 = "??"++(go rest) -- wildcard
        go (c:rest) = c:(go rest)
        go ""       = ""
        isSpecial c = c `elem` " #`\"\'\\|^&?*;~!-@:[]%{}><()$"

mapFile :: (String -> String) -> FilePath -> IO ()
mapFile f fp = do
  h <- openFile fp ReadMode
  contents <- hGetContents h
  let res = map f (lines contents)
  length res `seq` hClose h
  removeFile fp
  writeFile fp (unlines res)

rsyncCommand :: Bool -> FilePath -> FilePath -> String
rsyncCommand quiet src dst = intercalate " "
 (rsyncCommandName:(rsyncOptions quiet ++ map escape [src, dst] ++ ["\n"]))

rsyncCommandName :: String
rsyncCommandName = "rsync"

rsyncOptions :: Bool -> [String]
rsyncOptions quiet = ["-t", "-u", "-a", "-r",
  "--out-format='%i %f%L'", "--modify-window=2",
  "--no-perms"] ++ [if quiet then "--quiet" else "-vv"]
-- --modify-window=2 allows us to not re-copy files whose mod times
-- differ by up to 2 seconds. this is important because FAT filesystems
-- have a 2-second resolution
-- add -vi to the rsync options for debugging

uniqify :: FilePath -> IO FilePath
uniqify fp = do
  ex <- doesFileExist fp
  if ex then go 1 fp
    else return fp
       where go :: Integer -> FilePath -> IO FilePath
             go n path = do
               let perhaps = (dropExtension path ++ "_" ++ show n)
                              <.> (takeExtension path)
               ex <- doesFileExist perhaps
               if ex then go (n + 1) path
                 else return perhaps
 
mkSysTempFileName :: String -> IO (String,Handle)
mkSysTempFileName s = do
  parent <- getTemporaryDirectory
  openTempFile parent ("hplaylist_" ++ s)
  
removeAFile :: [Flag] -> FilePath -> IO ()
removeAFile fs _ | keepTmpFiles fs = return ()
removeAFile fs fp = do (unless (isQuiet fs) (putStrLn ("Deleting file: " ++ fp)))
                       removeFile fp

deleteFiles :: [Flag] -> [FilePath] -> IO ()
deleteFiles fs = mapM_ (removeAFile fs)

removeFilesOnExit :: [Flag] -> [FilePath] -> IO ()
removeFilesOnExit fs _ | isDryRun fs = return ()
removeFilesOnExit fs fps = deleteFiles fs fps