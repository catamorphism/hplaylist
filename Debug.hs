module Debug where

debug :: String -> IO ()
debug s | dEBUG = putStrLn ("===\n" ++ s ++ "===")
debug _ = return ()

dEBUG :: Bool
dEBUG = False

