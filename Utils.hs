module Utils(escape, mapFile) where

import Data.Char
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
