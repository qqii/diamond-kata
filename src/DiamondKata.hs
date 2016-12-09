module DiamondKata
    ( validChar
    , diamondKata
    ) where

import Data.Char (ord, chr, toUpper, isAlpha)

validChar :: Char -> Bool
validChar = isAlpha

charWidth :: Char -> Int
charWidth char = (ord char - ord 'A') * 2 + 1

kataLine :: Char -> String
kataLine 'A'  = "A"
kataLine char = [char] ++ spaces ++ [char]
  where
    spaces = replicate (charWidth char - 2) ' '

kataTop :: Char -> [String]
kataTop char = fmap (kataLine) ['A'..pchar]
  where
    pchar = chr (ord char - 1)

kataBottom :: Char -> [String]
kataBottom = reverse . kataTop

kataList :: Char -> [String]
kataList char = kataTop char ++ [kataLine char] ++ kataBottom char

diamondKata :: Char -> String
diamondKata char = unlines $ kataList (toUpper char)
