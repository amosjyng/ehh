import System.IO
import System.Environment
import Data.List.Split

-- DATA STRUCTURES

-- | Every Word is a String
type Word = String

-- | Check if both words in a tuple match each other
wordsMatch :: (Word, Word) -> Bool
wordsMatch wordPair = correctWord == studentWord
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
