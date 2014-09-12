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
wordsMatch :: [Word] -> (Word, Word) -> Bool
wordsMatch dictionaryWords wordPair
        | correctWord == studentWord = True
        | elem studentWord dictionaryWords = False
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
        let correctAnswer = splitOn " " $ args !! 0
        let studentAnswer = splitOn " " $ args !! 1
        print $ all (wordsMatch $ lines dict) $ zip correctAnswer studentAnswer
