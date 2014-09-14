module Main where

import Grade
import Test.Hspec

main :: IO ()
main = do
        dict <- readFile "dict.txt"
        let dictWords = map toLowerStr $ lines dict
	hspec $ do
        describe "Validate passing answers" $ do
                it "Match should be verified exactly" $ do
                        grade dictWords "house" "house"
                              `shouldBe` (True, Perfect, [])
