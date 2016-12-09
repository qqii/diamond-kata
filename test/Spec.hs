import Data.Char (ord, toLower)
import Test.QuickCheck
import DiamondKata

newtype AlphaChar = AlphaChar { unAlphaChar :: Char }

instance Arbitrary AlphaChar where
  arbitrary = AlphaChar <$> genAlphaChar

instance Show AlphaChar where
  show ch = show $ unAlphaChar ch

genAlphaChar :: Gen Char
genAlphaChar = elements $ ['a'..'z'] ++ ['A'..'Z']

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
correctLines (AlphaChar char) = (ord (toLower char) - ord 'a') * 2 + 1

testCorrectLines :: AlphaChar -> Bool
testCorrectLines alchar = kataLineCount alchar == correctLines alchar

testReverseLines :: AlphaChar -> Bool
testReverseLines alchar = let kl = kataLines alchar in kl == reverse kl

testLinesNonEmpty :: AlphaChar -> Bool
testLinesNonEmpty alchar = all notEmpty (kataLines alchar)

tests :: [AlphaChar -> Bool]
tests = [ testNonEmpty
        , testOddLines
        , testCorrectLines
        , testReverseLines
        , testLinesNonEmpty
        ]

main :: IO ()
main = mapM_ quickCheckResult tests
