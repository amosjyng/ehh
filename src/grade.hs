module Main (main) where

import System.IO
import System.Environment
import Data.List.Split
import Data.Char

-- DATA STRUCTURES

-- | Every Word is a String
type Word = String

-- | Convert an entire string to lowercase
toLowerStrings :: [String] -> [String]
toLowerStrings = map $ map toLower

-- | If the first two letters of each word are swapped, returns minimum edit
-- distance of the rest of the words. If the first two letters are not swapped, 
-- returns maxBound to ignore the swap
swapDistance :: Word -> Word -> Int
swapDistance word1 word2
        | length word1 < 2 || length word2 < 2 = maxBound -- ignore the swap
        | (word1 !! 0 == word2 !! 1) && (word1 !! 1 == word2 !! 0) =
                -- just return editDistance because +1 handled by editDistance
                editDistance (drop 2 word1) (drop 2 word2)
        | otherwise = maxBound -- not a swap, so ignore this

-- | Get the Damerau-Levenshtein distance between two words
editDistance :: Word -> Word -> Int
editDistance word1 word2
        | word1 == word2 = 0
        | null word1 = length word2
        | null word2 = length word1
        | head word1 == head word2 = editDistance (tail word1) (tail word2)
        | otherwise =
                1 + minimum [editDistance word1 (tail word2), -- add
                             editDistance (tail word1) word2, -- delete
                             editDistance (tail word1) (tail word2), -- replace
                             swapDistance word1 word2] -- swap

-- | Check if both words in a tuple are exact matches. If not, check that
-- 1. The first word is not in the dictionary words, and
-- 2. The first word is within 1 edit distance of the second
wordsMatch :: [Word] -> (Word, Word) -> Bool
wordsMatch dictionaryWords wordPair
        | correctWord == studentWord = True
        | studentWord `elem` dictionaryWords = False
        | otherwise = editDistance correctWord studentWord == 1
        where correctWord = fst wordPair
              studentWord = snd wordPair

-- | From commandline arguments, see if the student answer reasonably matches
-- the expected correct answer.
--
-- Usage: grade <correct answer> <student answer> <dictionary file>
main :: IO ()
main = do
        args <- getArgs
        dict <- readFile $ args !! 2
        let correctAnswer = toLowerStrings $ splitOn " " $ args !! 0
        let studentAnswer = toLowerStrings $ splitOn " " $ args !! 1
        let dictWords = toLowerStrings $ lines dict
        print $ all (wordsMatch dictWords) $ zip correctAnswer studentAnswer
