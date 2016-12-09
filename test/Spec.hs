import Data.Char (ord, chr, toUpper)
import Test.QuickCheck
import DiamondKata

genAlphaChar :: Gen Char
genAlphaChar = elements $ ['a'..'z'] ++ ['A'..'Z']

newtype AlphaChar = AlphaChar { unAlphaChar :: Char }

instance Arbitrary AlphaChar where
  arbitrary = AlphaChar <$> genAlphaChar
  shrink (AlphaChar 'A') = []
  shrink (AlphaChar char) = [AlphaChar pchar]
    where
      pchar = chr ((ord . toUpper) char - 1)

instance Show AlphaChar where
  show ch = show $ unAlphaChar ch

notEmpty :: String -> Bool
notEmpty str = length str /= 0

testNonEmpty :: AlphaChar -> Bool
testNonEmpty (AlphaChar char) = (notEmpty . diamondKata) char

kataLines :: AlphaChar -> [String]
kataLines = lines . diamondKata . unAlphaChar

kataLineCount :: AlphaChar -> Int
kataLineCount = length . kataLines

testOddLines :: AlphaChar -> Bool
testOddLines alchar = mod (kataLineCount alchar) 2 == 1

correctLines :: AlphaChar -> Int
correctLines (AlphaChar char) = (ord (toUpper char) - ord 'A') * 2 + 1

testCorrectLineNo :: AlphaChar -> Bool
testCorrectLineNo alchar = kataLineCount alchar == correctLines alchar

testReverseLines :: AlphaChar -> Bool
testReverseLines alchar = let kl = kataLines alchar in kl == reverse kl

testLinesNonEmpty :: AlphaChar -> Bool
testLinesNonEmpty alchar = all notEmpty (kataLines alchar)

testLinesCorrectChar :: AlphaChar -> Bool
testLinesCorrectChar alchar@(AlphaChar char) = all correctLetter (zip (kataLines alchar) ['A'..(toUpper char)])
  where
    correctLetter (l, c) = head l == c

testLinesCharsNo :: AlphaChar -> Bool
testLinesCharsNo alchar@(AlphaChar char) = all correctCharNo (zip (kataLines alchar) ['A'..(toUpper char)])
  where
    correctCharNo (l, c) = length l == correctLines (AlphaChar c)

testLinesReverse :: AlphaChar -> Bool
testLinesReverse alchar = all reversable (kataLines alchar)
  where
    reversable line = line == reverse line

tests :: [AlphaChar -> Bool]
tests = [ testNonEmpty
        , testOddLines
        , testCorrectLineNo
        , testReverseLines
        , testLinesNonEmpty
        , testLinesCorrectChar
        , testLinesCharsNo
        , testLinesReverse
        ]

main :: IO ()
main = mapM_ quickCheckResult tests
