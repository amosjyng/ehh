module Main (main) where

import System.IO
import System.Environment
import Data.List (find)
import Data.List.Split
import Data.Char
import Data.Maybe (isNothing, fromJust)


-- | Convert an entire string to lowercase
toLowerStr :: String -> String
toLowerStr = map toLower

-- | How does this word compare to the gold standard word?
data Result = Perfect | Typo | Missing | Extra | WrongWord deriving (Eq)

-- | Print a result like in the examples
instance Show Result where
        show Perfect = "None"
        show Typo = "typo"
        show Missing = "missing"
        show Extra = "extra"
        show WrongWord = "wrong_word"

-- | Tells you whether a certain type of error is fine to have
isFine :: Result -> Bool
isFine r = r == Perfect || r == Typo

-- | Highlight corresponding portions of expected and user text
type Highlight = ((Int, Int), (Int, Int))

-- | Every Result other than Perfect should have a highlight to go with it
type ResultHighlight = (Result, Maybe Highlight) 

-- | Every Word is a String with a location
data Word = Word String (Int, Int) deriving (Show)

-- | Get the String that forms the Word
wordStr :: Word -> String
wordStr (Word str _) = str

-- | Get the range of the word in the sentence
range :: Word -> (Int, Int)
range (Word _ r) = r

-- | Two Words are equal if their strings (and not necessarily locations) are
-- equal
instance Eq Word where
        Word a _ == Word b _ = a == b

-- | Creates a list of Words one token at a time
formWordList :: (Bool, Int, [Word]) -> String -> (Bool, Int, [Word])
formWordList (delimiter, startPos, wordList) token
        | delimiter = (not delimiter, startPos + l, wordList)
        | otherwise = (not delimiter, startPos + l,
                       wordList ++ [Word token (startPos, startPos + l)])
        where l = length token

-- | Get the third element of a tuple
third :: (a, b, c) -> c
third (_, _, z) = z

-- | Split a sentence into lowercase Words delimited by spaces and punctuation
splitIntoWords :: String -> [Word]
splitIntoWords sentence = third $ foldl formWordList (False, 0, []) tokens
        where tokens = split (condense $ oneOf " .") sentence

-- | If the first two letters of each string are swapped, returns minimum edit
-- distance of the rest of the strings. If the first two letters are not 
-- swapped, returns maxBound to ignore the swap
swapDistance :: String -> String -> Int
swapDistance word1 word2
        | length word1 < 2 || length word2 < 2 = maxBound -- ignore the swap
        | (word1 !! 0 == word2 !! 1) && (word1 !! 1 == word2 !! 0) =
                -- just return editDistance because +1 handled by editDistance
                editDistance (drop 2 word1) (drop 2 word2)
        | otherwise = maxBound -- not a swap, so ignore this

-- | Get the Damerau-Levenshtein distance between two strings
editDistance :: String -> String -> Int
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
--
-- If there's anything of note (typo, etc.) the highlight will contain the range
-- of the interesting part
wordsMatch :: [String] -> (Word, Word) -> ResultHighlight
wordsMatch dictionaryWords wordPair
        | correctWord == studentWord = (Perfect, Nothing)
        | wordStr studentWord `elem` dictionaryWords =
                (WrongWord, Just highlight)
        | editDistance (wordStr correctWord) (wordStr studentWord) == 1 =
                (Typo, Just highlight)
        | otherwise = (WrongWord, Just highlight)
        where correctWord = fst wordPair
              studentWord = snd wordPair
              highlight = (range correctWord, range studentWord)

-- | Collect all highlights of a certain type
collectHighlights :: Result -> [ResultHighlight] -> [Highlight]
collectHighlights r = (map (fromJust. snd)) . (filter (\rh -> fst rh == r))

-- | Find the most relevant highlights to show
findRelevantHighlights :: [ResultHighlight] -> (Result, [Highlight])
findRelevantHighlights highlights
        | isNothing imperfects = (Perfect, [])
        | otherwise = fromJust imperfects
        where typos = (Typo, collectHighlights Typo highlights)
              missings = (Missing, collectHighlights Missing highlights)
              extras = (Extra, collectHighlights Extra highlights)
              wrongs = (WrongWord, collectHighlights WrongWord highlights)
              imperfects = find (not . null . snd)
                                [missings, extras, wrongs, typos]

-- | From commandline arguments, see if the student answer reasonably matches
-- the expected correct answer.
--
-- Usage: grade <correct answer> <student answer> <dictionary file>
main :: IO ()
main = do
        args <- getArgs
        dict <- readFile $ args !! 2
        let correctAnswer = splitIntoWords $ args !! 0
        let studentAnswer = splitIntoWords $ args !! 1
        let dictWords = map toLowerStr $ lines dict
        let results = map (wordsMatch dictWords)
                        $ zip correctAnswer studentAnswer
        let relevantHighlights = findRelevantHighlights results
        print $ ((all isFine $ map fst results),
                 (fst relevantHighlights),
                 (snd relevantHighlights)) 
