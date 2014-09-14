module Main where

import Grade
import Test.Hspec

main :: IO ()
main = do
        dict <- readFile "dict.txt"
        let dictWords = map toLowerStr $ lines dict
	hspec $ do
        describe "Validate perfect answers" $ do
                it "The exact same word should pass" $ do
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
			grade dictWords "Yet\" \" ??another house!" "yeT,ANOTHER.\"HoUSE"
				`shouldBe` (True, Perfect, [])
	describe "Validate typo answers" $ do
		it "An edit distance of 1 should be acceptable" $ do
			grade dictWords "house" "hhouse"
				`shouldBe` (True, Typo, [((0,5),(0,6))])
		it "Punctuation should be ignored in typos" $ do
			grade dictWords "`house?!" "\"hhouse:"
				`shouldBe` (True, Typo, [((1,6),(1,7))])
		it "Multiple typos should all be recorded" $
			grade dictWords "This is my house!" " this is, mp hhouse."
				`shouldBe` (True, Typo, [((8,10),(10,12)),((11,16),(13,19))])
