{-
      TODO
       * might be good to copy files using system calls rather than by creating a shell script. (Wouldn't have to use wildcards for Unicode characters)
       * flag to delete files
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

import Prelude hiding (catch)
import Control.Exception
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
import Debug
import Utils
import Flags

playlistsToFilenames :: [Flag] -> [String] -> IO [(FilePath,[FilePath])]
playlistsToFilenames flags fs = do
    tmpD <- tempDir flags
    mapM (go tmpD) fs
  where go tmpD playListName = do
          outFile <- uniqify $ tmpD </> playListName <.> "m3u"
          let script = playlistScript playListName outFile
          debug script
          (appleScriptName,hdl_as) <- mkSysTempFileName "applescript"
          hPutStrLn hdl_as script
          hClose hdl_as
          makeExecutable appleScriptName
          exitCode <- system appleScriptName
          removeAFile flags appleScriptName
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

mkTempFileName :: [Flag] -> String -> IO (String,Handle)
mkTempFileName flags s = do
  parent <- tempDir flags
  parentExists <- doesDirectoryExist parent
  putStrLn ("mkTempFileName: " ++ show (parent, parentExists))
  unless parentExists
    (throwIO (userError ("directory doesn't seem to exist: " ++ parent)))
  outF <- uniqify $ parent </> ("hplaylist_" ++ s)
  h <- openFile outF WriteMode
  return (outF, h) 

albumArtistTrack :: FilePath -> (FilePath,FilePath,FilePath)
albumArtistTrack fn = (case reverse parts of
  (track:album:artist:_) -> (artist,album,track)
  _                      -> error ("error parsing filename " ++ show fn))
   where parts = splitPath fn

generateCopyScript :: [Flag] -> [String] -> IO ()
generateCopyScript flags playlistNames = do
  allMusicFiles <- playlistsToFilenames flags playlistNames
  let playlistFileNames = fst (unzip allMusicFiles) 
  (do debug (show allMusicFiles)
      mapM_ fixPaths (fst (unzip allMusicFiles))
      (copyScriptName,hdl) <- mkTempFileName flags "copyit"
      let copyScript = "#!/bin/bash\n" ++ concatMap (\ (playlistFn,songs) -> 
                        (rsyncCommand (isQuiet flags) playlistFn musicPlayerPlaylistRoot ++
                        concatMap (\ s -> let (artist,album,_) = albumArtistTrack s
                                              parent = musicPlayerRoot </> musicSubdir </> artist </> album in
                                              "mkdir -p " ++ (escape parent) ++ "\n"
                                              ++ rsyncCommand (isQuiet flags) s parent) songs))
                         allMusicFiles
      hPutStrLn hdl copyScript
      hClose hdl
      (do makeExecutable copyScriptName
          if (isDryRun flags)
             then putStrLn ("To copy the files, execute the script " ++ copyScriptName)
             else do
               res <- system copyScriptName
               removeFilesOnExit flags (copyScriptName:playlistFileNames)
               case res of
                 ExitFailure _ -> putStrLn ("An error occurred: " ++ show res) >>
                            exitWith res
                 _ -> putStrLn ("Processed " ++ show (sum (map (length . snd)
                          allMusicFiles)) ++ " files")) 
            `catch` (myHandler flags copyScriptName))
   `onException` (deleteFiles flags playlistFileNames)

myHandler :: [Flag] -> FilePath -> IOException -> IO a       
myHandler flags copyScriptName ex = do
    removeAFile flags copyScriptName
    throw ex

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
         [] -> putStrLn (usageInfo "hplaylist" options) >>
               exitWith (ExitFailure 1)
         _  -> debug ("playlists = " ++ show playlistNames) >>
                 generateCopyScript opts playlistNames

