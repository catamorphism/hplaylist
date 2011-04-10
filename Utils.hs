module Utils(escape) where

import Data.Char

escape :: String -> String
escape s = go s
  where go (c:rest) | isSpecial c = "\\"++[c]++(go rest)
        go (c:rest) = c:(go rest)
        go ""       = ""
        isSpecial c = c `elem` " #`\"\'\\|^&?*;~!-@:[]%{}><()$"
                      || ord c >= 128
