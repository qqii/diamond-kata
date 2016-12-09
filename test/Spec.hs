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

noLines :: String -> Int
noLines str = (length . lines) str

testOddLines :: AlphaChar -> Bool
testOddLines (AlphaChar char) = mod ((noLines . diamondKata) char) 2 == 1

correctLines :: Char -> Int
correctLines char = (ord (toLower char) - ord 'a') * 2 + 1

testCorrectLines :: AlphaChar -> Bool
testCorrectLines (AlphaChar char) = (noLines . diamondKata) char == correctLines char

testReverseLines :: AlphaChar -> Bool
testReverseLines (AlphaChar char) = result == reverseResult
  where
    result = diamondKata char
    reverseResult = (unlines . reverse . lines) result

testLinesNonEmpty :: AlphaChar -> Bool
testLinesNonEmpty (AlphaChar char) = all notEmpty ((lines . diamondKata) char)

tests :: [AlphaChar -> Bool]
tests = [ testNonEmpty
        , testOddLines
        , testCorrectLines
        , testReverseLines
        , testLinesNonEmpty
        ]

main :: IO ()
main = mapM_ quickCheckResult tests
