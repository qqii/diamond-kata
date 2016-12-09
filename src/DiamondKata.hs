module DiamondKata
    ( validChar
    , diamondKata
    ) where

import Data.Char (ord, chr, toUpper, isAlpha)

validChar :: Char -> Bool
validChar = isAlpha

diamondKataLine :: Char -> String
diamondKataLine 'A'  = "A"
diamondKataLine char = [char] ++ (replicate (n-2) ' ') ++ [char]
  where
    n = (ord char - ord 'A') * 2 + 1

diamondKataListMirror :: Char -> [String]
diamondKataListMirror char = fmap (diamondKataLine) ['A'..pchar]
  where
    pchar = chr (ord char - 1)

diamondKataList :: Char -> [String]
diamondKataList char = mir ++ [diamondKataLine char] ++ (reverse mir)
  where
    mir = diamondKataListMirror char

diamondKata :: Char -> String
diamondKata char = unlines $ diamondKataList (toUpper char)
