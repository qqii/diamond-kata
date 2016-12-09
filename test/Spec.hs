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

testNonEmpty :: AlphaChar -> Bool
testNonEmpty (AlphaChar char) = length (diamondKata char) /= 0

noLines :: String -> Int
noLines str = (length . lines) str

testOddLines :: AlphaChar -> Bool
testOddLines (AlphaChar char) = mod ((noLines . diamondKata) char) 2 == 1

correctLines :: AlphaChar -> Int
correctLines (AlphaChar char) = (ord (toLower char) - ord 'a') * 2 + 1

testCorrectLines :: AlphaChar -> Bool
testCorrectLines alChar@(AlphaChar char) = (noLines . diamondKata) char == correctLines alChar

tests :: [AlphaChar -> Bool]
tests = [ testNonEmpty
        , testOddLines
        , testCorrectLines
        ]

main :: IO ()
main = mapM_ quickCheckResult tests
