import System.IO
import System.Environment
import Data.List.Split

-- DATA STRUCTURES

-- | Every Word is a String
type Word = String

-- | Get the edit distance between two words
editDistance :: Word -> Word -> Int
editDistance word1 word2
        | word1 == word2 = 0
        | null word1 = length word2
        | null word2 = length word1
        | head word1 == head word2 = editDistance (tail word1) (tail word2)
        | otherwise =
                1 + minimum [editDistance word1 (tail word2), -- add
                             editDistance (tail word1) word2, -- delete
                             editDistance (tail word1) (tail word2)] -- replace

-- | Check if both words in a tuple match each other
wordsMatch :: (Word, Word) -> Bool
wordsMatch wordPair
        | correctWord == studentWord = True
        | otherwise = editDistance correctWord studentWord == 1
        where correctWord = fst wordPair
              studentWord = snd wordPair

-- | From commandline arguments, see if the student answer reasonably matches
-- the expected correct answer
main :: IO ()
main = do
        args <- getArgs
        let correctAnswer = splitOn " " $ head args
        let studentAnswer = splitOn " " $ last args
        print $ all wordsMatch $ zip correctAnswer studentAnswer
