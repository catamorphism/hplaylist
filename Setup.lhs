#!/usr/bin/env runhaskell
\begin{code}
{-# OPTIONS -Wall -cpp #-}

import Data.List
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple
import System.Console.GetOpt

main :: IO ()
main = do
   let hooks = simpleUserHooks {
                 buildHook = buildHook simpleUserHooks,
                 preConf   = generateConfigHs
            }
   defaultMainWithHooks hooks

data MyFlag = MusicPlayerRoot FilePath
            | M4ADir FilePath
  deriving (Eq, Show)

myOptions :: [OptDescr MyFlag]
myOptions = 
  [Option [] ["music-player-root"] (ReqArg MusicPlayerRoot "DIRECTORY")
     ("Top-level directory for your music player."),
  Option [] ["m4a-dir"] (ReqArg M4ADir "DIRECTORY")
  ("Root cache directory in which to find MP3 files for M4A files")]

findRoot :: [MyFlag] -> Maybe FilePath
findRoot fs = foldl' (\ rest foo ->
                          case foo of
                            MusicPlayerRoot fp -> Just fp
                            _ -> rest) Nothing fs

findM4a :: [MyFlag] -> Maybe FilePath
findM4a fs = foldl' (\ rest foo ->
                          case foo of
                            M4ADir fp -> Just fp
                            _ -> rest) Nothing fs

generateConfigHs :: Args -> ConfigFlags -> IO HookedBuildInfo
generateConfigHs _ confFlags = do
  let userConfigArgs = configConfigureArgs confFlags
  case getOpt Permute myOptions userConfigArgs of
    (opts,_,_) ->
      case findRoot opts of
        Just dir -> let cacheDir = findM4a opts in
                       (report dir cacheDir) >>
                       (writeConfigHs dir (findM4a opts))
        Nothing -> (error $ "You must specify the destination "
                     ++ "directory for your music player, like so:\n"
                     ++ "--configure-option=--music-player-root="
                     ++ "/Volumes/SANSA\\ CLIPP")
  return emptyHookedBuildInfo

report :: FilePath -> Maybe FilePath -> IO ()
report root maybeCacheDir = do
  putStrLn ("Set music player root directory to: " ++ root)
  maybe (return ()) (\ fp ->
    putStrLn ("Set .mp3 cache directory to: " ++ fp)) maybeCacheDir

writeConfigHs :: FilePath -> Maybe FilePath -> IO ()
writeConfigHs dir maybeDir = writeFile "Config.hs" 
  ("module Config where\nimport System.FilePath\nmusicPlayerRoot :: FilePath\nmusicPlayerRoot = \"" ++ dir ++ "\"\n" ++ "m4aParentDir :: Maybe FilePath\nm4aParentDir = " ++ maybe "Nothing" (\ f -> "Just " ++ "\"" ++ f ++ "\"") maybeDir ++ "\n")
\end{code}