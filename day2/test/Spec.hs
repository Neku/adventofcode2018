import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "Lib.hasTwoRepeat" $ do
        it "should return false if there is no repeat" $ do
            hasTwoRepeat "abcde" `shouldBe` False
        it "should return true if there are two repeated letters" $ do
            hasTwoRepeat "abcda" `shouldBe` True
        it "should return true if there are three repeated letters" $ do
            hasTwoRepeat "abada" `shouldBe` False
        it "should return true if there are two repeated letters not first" $ do
            hasTwoRepeat "abbdd" `shouldBe` True
        it "should pass with example" $ do
            (length $ filter hasTwoRepeat input) `shouldBe` 4
    describe "Lib.hasThreeRepeat" $ do
        it "should return false if there is no repeat" $ do
            hasThreeRepeat "abcde" `shouldBe` False
        it "should return true if there are three repeated letters" $ do
            hasThreeRepeat "abada" `shouldBe` True
        it "should return true if there are four repeated letters" $ do
            hasTwoRepeat "abaaa" `shouldBe` False
        it "should pass with example" $ do
            (length $ filter hasThreeRepeat input) `shouldBe` 3
    describe "Lib.checksum" $ do
        it "should pass with the example" $ do
            checkSum input `shouldBe` 12
    describe "Lib.numberOfDifferentChars" $ do
        it "should say 0 if string are equals" $ do
            numberOfDifferentChars "abcde" "abcde" `shouldBe` 0
        it "should say 1 if one char is different" $ do
            numberOfDifferentChars "abcde" "afcde" `shouldBe` 1

input = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]