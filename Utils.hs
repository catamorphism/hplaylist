module Utils(escape, mapFile, rsyncCommand) where

import Data.Char
import Data.List
import System.IO
import System.Directory

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

rsyncCommand :: FilePath -> FilePath -> String
rsyncCommand src dst = intercalate " "
 (rsyncCommandName:(rsyncOptions ++ map escape [src, dst] ++ ["\n"]))

rsyncCommandName :: String
rsyncCommandName = "rsync"

rsyncOptions :: [String]
rsyncOptions = ["-t", "-u", "-a", "-r", "-vv", 
  "--out-format='%i %f%L'", "--modify-window=2",
  "--no-perms"]
-- --modify-window=2 allows us to not re-copy files whose mod times
-- differ by up to 2 seconds. this is important because FAT filesystems
-- have a 2-second resolution
-- add -vi to the rsync options for debugging
