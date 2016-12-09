module DiamondKata
    ( validChar
    , diamondKata
    ) where

import Data.Char (ord, chr, toUpper, isAlpha)

validChar :: Char -> Bool
validChar = isAlpha

charWidth :: Char -> Int
charWidth char = (ord char - ord 'A') * 2 + 1

kataLine :: Char -> Char -> String
kataLine kchar 'A'  = os ++ "A" ++ os
  where
    os = replicate (ord kchar - ord 'A') ' '
kataLine kchar char = os ++ [char] ++ ms ++ [char] ++ os
  where
    os = replicate (ord kchar - ord char) ' '
    ms = replicate (charWidth char - 2) ' '

kataTop :: Char -> [String]
kataTop char = fmap (kataLine char) ['A'..pchar]
  where
    pchar = chr (ord char - 1)

kataBottom :: Char -> [String]
kataBottom = reverse . kataTop

kataList :: Char -> [String]
kataList char = kataTop char ++ [kataLine char char] ++ kataBottom char

diamondKata :: Char -> String
diamondKata char = unlines $ kataList (toUpper char)
