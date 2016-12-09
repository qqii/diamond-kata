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

testOddLines :: AlphaChar -> Bool
testOddLines (AlphaChar char) = mod (length (diamondKata char)) 2 == 1

main :: IO ()
main = mapM_ quickCheck [testNonEmpty
                        ,testOddLines]
