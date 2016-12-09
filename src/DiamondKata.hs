module DiamondKata
    ( validChar
    , diamondKata
    ) where

import Data.Char (ord, chr, toUpper, isAlpha)

validChar :: Char -> Bool
validChar = isAlpha

diamondKataListMirror :: Char -> [String]
diamondKataListMirror char = fmap (:"") ['A'..pchar]
  where
    pchar = chr ((ord . toUpper) char - 1)

diamondKataList :: Char -> [String]
diamondKataList char = mir ++ [[toUpper char]] ++ (reverse mir)
  where
    mir = diamondKataListMirror char

diamondKata :: Char -> String
diamondKata char = unlines $ diamondKataList char
