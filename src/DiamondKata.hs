module DiamondKata
    ( validChar
    , diamondKata
    ) where

import Data.Char (ord, toLower, isAlpha)

validChar :: Char -> Bool
validChar = isAlpha

diamondKataList :: Char -> [String]
diamondKataList char = replicate ls "A"
  where
    ls = (ord (toLower char) - ord 'a') * 2 + 1

diamondKata :: Char -> String
diamondKata char = unlines $ diamondKataList char
