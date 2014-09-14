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
			grade dictWords "house" "house?"
				`shouldBe` (True, Perfect, [])
			grade dictWords "house" "\"house.\""
				`shouldBe` (True, Perfect, [])
	describe "Validate typo answers" $ do
		it "An edit distance of 1 should be acceptable" $ do
			grade dictWords "house" "hhouse"
				`shouldBe` (True, Typo, [((0,5),(0,6))])
