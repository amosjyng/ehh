module Main where

import Grade
import Test.Hspec

main :: IO ()
main = do
        dict <- readFile "dict.txt"
        let dictWords = map toLowerStr $ lines dict
        hspec $ do
        describe "Validate perfect answers" $ do
                it "The exact same word should pass" $
                        grade dictWords "house" "house"
                                `shouldBe` (True, Perfect, [])
                it "The exact same word with punctuation should pass" $ do
                        grade dictWords "house." "house"
                                `shouldBe` (True, Perfect, [])
                        grade dictWords "house" "house,"
                                `shouldBe` (True, Perfect, [])
                        grade dictWords "house!" "house"
                                `shouldBe` (True, Perfect, [])
                        grade dictWords "house." "house?"
                                `shouldBe` (True, Perfect, [])
                        grade dictWords "house" "\"house.\""
                                `shouldBe` (True, Perfect, [])
                        grade dictWords "it's" "it's" -- it's vs its
                                `shouldBe` (True, Perfect, [])
                it "Multiple words should pass" $ do
                        grade dictWords "A house" "a house."
                                `shouldBe` (True, Perfect, [])
                        grade dictWords "Yet\" \" ??another house!"
                                        "yeT,ANOTHER.\"HoUSE"
                                `shouldBe` (True, Perfect, [])
                it "Should have Unicode support" $
                        grade dictWords "über is not English"
                                        "über is not English"
                                `shouldBe` (True, Perfect, [])
        describe "Validate typo answers" $ do
                it "An edit distance of 1 should be acceptable" $ do
                        grade dictWords "house" "hhouse" -- addition
                                `shouldBe` (True, Typo, [((0,5),(0,6))])
                        grade dictWords "house" "huse" -- deletion
                                `shouldBe` (True, Typo, [((0,5),(0,4))])
                        grade dictWords "house" "hause" -- replacement
                                `shouldBe` (True, Typo, [((0,5),(0,5))])
                        grade dictWords "house" "huose" -- replacement
                                `shouldBe` (True, Typo, [((0,5),(0,5))])
                it "Punctuation should be ignored in typos" $
                        grade dictWords "`house?!" "\"hhouse:"
                                `shouldBe` (True, Typo, [((1,6),(1,7))])
                it "Multiple typos should all be recorded" $
                        grade dictWords "This is my house!" " this is, mp hhouse."
                                `shouldBe` (True, Typo, [((8,10),(10,12)),((11,16),(13,19))])
                it "Should have Unicode support" $
                        grade dictWords "über is not English" "übr is not English"
                                `shouldBe` (True, Typo, [((0,4),(0,3))])
        describe "Invalidate wrong word answers" $ do
                it "A completely different word should be wrong" $ do
                        grade dictWords "This is my house." "this is your house!"
                                `shouldBe` (False, WrongWord, [((8,10),(8,12))])
                        grade dictWords "This is my house." "this is mpr house!"
                                `shouldBe` (False, WrongWord, [((8,10),(8,11))])
                it "More than one wrong word means nothing is highlighted" $ do
                        grade dictWords "That is my house." "This is your house!"
                                `shouldBe` (False, Perfect, [])
                        grade dictWords "That is my house." "This is your horse!"
                                `shouldBe` (False, Perfect, [])
                it "A similar word that is also valid is wrong" $
                        grade dictWords "It's my house." "Its my house!"
                                `shouldBe` (False, WrongWord, [((0,4),(0,3))])
                it "When a wrong word exists, ignore any typos" $
                        grade dictWords "It's my house" "It's mp horse"
                                `shouldBe` (False, WrongWord, [((8,13),(8,13))])
                it "Should have Unicode support" $ do
                        grade dictWords "über is not English"
                                        "über is not Engrsh"
                                `shouldBe` (False, WrongWord, [((12,19),(12,18))])
                        grade dictWords "über 不是 English"
                                        "über 不是 Engrsh"
                                `shouldBe` (False, WrongWord, [((8,15),(8,14))])
        describe "Invalidate missing words" $ do
                it "Missing input should be marked as such" $
                        grade dictWords "house" ""
                                `shouldBe` (False, Missing, [((0,5),(0,0))])
                it "Punctuation should be ignored" $
                        grade dictWords "house" ".,\"?"
                                `shouldBe` (False, Missing, [((0,5),(0,0))])
                it "Input with a missing word should be detected" $
                        grade dictWords "This is my house."
                                        "This my house!"
                                `shouldBe` (False, Missing, [((5,7),(5,5))])
                it "Multiple missing words shouldn't be highlighted" $
                        grade dictWords "This is my house." "This house!"
                                `shouldBe` (False, Perfect, [])
                it "Should be able to distinguish typos from missing words" $
                        grade dictWords "This is my house" "This si house"
                                `shouldBe` (False, Missing, [((8,10),(8,8))])
