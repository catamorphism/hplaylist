{-
      TODO
       * might be good to copy files using system calls rather than by creating a shell script. (Wouldn't have to use wildcards for Unicode characters)
       * flag to delete files
       * don't use system temp directory (have a flag to specify the name of the output script file)
       * add a --quiet flag
       * filter out some of the rsync output
       
-}

{- 
   Copyright (c) Timothy Chevalier, 2011
 
This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# OPTIONS -Wall #-}
module Main(main) where

import Control.Monad
import Data.Char
import System.Cmd
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import Applescript
import Config
import Utils

playlistsToFilenames :: [String] -> IO [(FilePath,[FilePath])]
playlistsToFilenames fs = do
    tmpD <- getTemporaryDirectory
    mapM (go tmpD) fs
  where go tmpD playListName = do
          let outFile = tmpD </> playListName <.> "m3u"
          let script = playlistScript playListName outFile
          debug script
          (appleScriptName,hdl_as) <- mkTempFileName "applescript"
          hPutStrLn hdl_as script
          hClose hdl_as
          makeExecutable appleScriptName
          exitCode <- system appleScriptName
          case exitCode of
            ExitSuccess -> do
              songFiles <- (liftM lines) $ readFile outFile
              rewriteM4a outFile
              return (outFile,
                      map m4aHack songFiles)
            _           -> error ("Failed to extract playlist "
                             ++ playListName ++ ": " ++ show exitCode)
        rewriteM4a = mapFile m4aHack 
        m4aHack fn | takeExtension fn == ".m4a" =
          case m4aParentDir of
            Just d ->
                replaceExtension
                (d </> (joinPath (drop 6 (splitPath fn))))
                "mp3"
            Nothing -> fn
        m4aHack fn = fn

makeExecutable :: FilePath -> IO ()
makeExecutable fn = do
  perms <- getPermissions fn
  setPermissions fn (perms{executable=True})

musicPlayerPlaylistRoot :: FilePath
musicPlayerPlaylistRoot = musicPlayerRoot </> "Playlists"
musicSubdir :: FilePath
musicSubdir = "Music"

mkTempFileName :: String -> IO (String,Handle)
mkTempFileName s = do
  tmp <- getTemporaryDirectory
  openTempFile tmp ("hplaylist_" ++ s)

albumArtistTrack :: FilePath -> (FilePath,FilePath,FilePath)
albumArtistTrack fn = (case reverse parts of
  (track:album:artist:_) -> (artist,album,track)
  _                      -> error ("error parsing filename " ++ show fn))
   where parts = splitPath fn

generateCopyScript :: Bool -> [String] -> IO ()
generateCopyScript isDryRun playlistNames = do
  allMusicFiles <- playlistsToFilenames playlistNames
  debug (show allMusicFiles)
  mapM_ fixPaths (fst (unzip allMusicFiles))
  (copyScriptName,hdl) <- mkTempFileName "copyit"
  let copyScript = "#!/bin/bash\n" ++ concatMap (\ (playlistFn,songs) -> 
                        (rsyncCommand playlistFn musicPlayerPlaylistRoot ++
                        concatMap (\ s -> let (artist,album,_) = albumArtistTrack s
                                              parent = musicPlayerRoot </> musicSubdir </> artist </> album in
                                              "mkdir -p " ++ (escape parent) ++ "\n"
                                              ++ rsyncCommand s parent) songs))
                         allMusicFiles
  hPutStrLn hdl copyScript
  hClose hdl
  makeExecutable copyScriptName
  if isDryRun
     then putStrLn ("To copy the files, execute the script " ++ copyScriptName)
     else do
       res <- system copyScriptName
       case res of
          -- exit 1 just means that cp -n didn't copy a file that already existed
         ExitFailure n | n /= 1 -> putStrLn ("An error occurred: " ++ show res) >>
                          exitWith res
         _ -> putStrLn ("Copied " ++ show (sum (map (length . snd)
                          allMusicFiles)) ++ " files")
       
-- :-(
windowsPath :: FilePath -> FilePath
windowsPath = map (\ c -> if c == '/' then '\\' else c)

fixPaths :: FilePath -> IO ()
fixPaths fn = do
  debug ("fixPaths: " ++ fn)
  hdl <- openFile fn ReadMode
  tracks <- (liftM lines) (hGetContents hdl)
  let new = map (\ fp -> let (artist,album,track) = albumArtistTrack fp in
                                             -- Sansa Clip requires a filepath relative to the root, like:
                                             -- Music\Billy Bragg\Don't Try This At Home\Sexuality
                                             -- You can't leave off the Music\ or have a \ at the beginning.
                                             -- You may need to adjust the format for other players.
                           windowsPath $ joinPath [musicSubdir,artist,album,track]) tracks
-- The seq is important, else we'll delete the file before we read it
  length new `seq` hClose hdl
  removeFile fn
  debug ("contents = " ++ show tracks)
  debug ("result = " ++ unlines new)
  hdlOut <- openFile fn WriteMode
  hPutStrLn hdlOut (unlines new)
  hClose hdlOut

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (opts, playlistNames, _) ->
       case playlistNames of
         [] -> putStrLn "You didn't give me any arguments, and that's fine!"
         _  -> let isDryRun = getDryRun opts in
                  debug ("playlists = " ++ show playlistNames) >>
                    generateCopyScript isDryRun playlistNames

options :: [OptDescr Flag]
options = [Option ['d'] ["dry-run"] (NoArg DryRun)
  "Don't copy any files to the music player, just generate a script to do so"]

data Flag = DryRun

getDryRun :: [Flag] -> Bool
getDryRun (DryRun:_) = True
getDryRun _          = False

debug :: String -> IO ()
debug s | dEBUG = putStrLn ("===\n" ++ s ++ "===")
debug _ = return ()

dEBUG :: Bool
dEBUG = False

