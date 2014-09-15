module Grade (toLowerStr, Result(..), grade) where

import           Data.Char
import           Data.Function (on)
import           Data.List       (find, minimumBy)
import           Data.List.Split
import           Data.Maybe      (fromJust, isNothing)


-- | Convert an entire string to lowercase
toLowerStr :: String -> String
toLowerStr = map toLower

-- | How does this word compare to the gold standard word?
data Result = Perfect | Typo | Missing | WrongWord deriving (Eq)

-- | Print a result like in the examples
instance Show Result where
        show Perfect = "None"
        show Typo = "typo"
        show Missing = "missing"
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
                       wordList ++
                        [Word (toLowerStr token) (startPos, startPos + l)])
        where l = length token

-- | Get the third element of a tuple
third :: (a, b, c) -> c
third (_, _, z) = z

-- | Split a sentence into lowercase Words delimited by spaces and punctuation
splitIntoWords :: String -> [Word]
splitIntoWords sentence = filter (not . null . wordStr) sentenceWords
        where tokens = split (condense $ oneOf " !\"#$%&()*+,-./:;<=>?@[\\]^_`{|}~") sentence
              sentenceWords = third $ foldl formWordList (False, 0, []) tokens

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

-- | Check if both words are exact matches. If not, check that
-- 1. The first word is not in the dictionary words, and
-- 2. The first word is within 1 edit distance of the second
--
-- If there's anything of note (typo, etc.) the highlight will contain the range
-- of the interesting part
wordsMatch :: [String] -> Word -> Word -> ResultHighlight
wordsMatch dictionaryWords correctWord studentWord
        | correctWord == studentWord = (Perfect, Nothing)
        | wordStr studentWord `elem` dictionaryWords =
                (WrongWord, Just highlight)
        | editDistance (wordStr correctWord) (wordStr studentWord) == 1 =
                (Typo, Just highlight)
        | otherwise = (WrongWord, Just highlight)
        where highlight = (range correctWord, range studentWord)

-- | Collect all highlights of a certain type
collectHighlights :: Result -> [ResultHighlight] -> [Highlight]
collectHighlights r = map (fromJust . snd) . filter (\rh -> fst rh == r)

-- | Check if there are multiple wrong things (missing or wrong words) and if
-- so, return no highlights. Otherwise return highlights as they are
multipleWrongs :: (Result, [Highlight]) -> (Result, [Highlight])
multipleWrongs (Missing,   _:_:_) = (Perfect, [])
multipleWrongs (WrongWord, _:_:_) = (Perfect, [])
multipleWrongs rhs = rhs

-- | Find the most relevant highlights to show
findRelevantHighlights :: [ResultHighlight] -> (Result, [Highlight])
findRelevantHighlights highlights
        | isNothing imperfects = (Perfect, [])
        | otherwise = (multipleWrongs . fromJust) imperfects
        where byResult = map (\r -> (r, collectHighlights r highlights))
                             [Missing, WrongWord, Typo]
              imperfects = find (not . null . snd) byResult

-- | Finds results for all words, figuring out along the way if it's a word is
-- missing or just wrong
getResults :: [String] -> (Int, [ResultHighlight]) -> [Word] -> [Word] -> Int -> (Int, [ResultHighlight])
getResults dictWords (d, resultHs) cToks sToks sPos
        | null cToks = (d, resultHs) -- we're all done
        | null sToks = newMissing -- last word is missing
        -- either perfect or a typo, carry on
        | isFine $ fst resultH = recurse (d, snd newResult) restST newSPos
        -- decide wrong/missing word by comparing sentence edit distance
        | otherwise = minimumBy (compare `on` fst)
                                [recurse newResult  restST newSPos,
                                 recurse newMissing sToks  sPos]
        where cTok:restCT = cToks -- correct tokens
              sTok:restST = sToks -- student tokens
              resultH  = wordsMatch dictWords cTok sTok
              missingH = (Missing, Just (range cTok, (sPos, sPos)))
              newResult  = (1 + d, resultHs ++ [resultH])
              newMissing = (1 + d, resultHs ++ [missingH])
              newSPos = (fst . range . head) restST
              recurse = \pc -> getResults dictWords pc restCT

-- | Given a list of words in the English language, an expected string, and an
-- actual student string, return a tuple indicating whether the student answer
-- matches the correct answer within acceptable bounds, what kind of mistakes
-- were made (if any), and where the mistakes are located
grade :: [String] -> String -> String -> (Bool, Result, [Highlight])
grade dictWords correctAnswer studentAnswer
        -- if missing more than two words, don't need to highlight results, so
        -- skip computation
        | length correctTokens > length studentTokens + 1 = (False, Perfect, [])
        | otherwise =
                (all isFine $ map fst results, fst highlights, snd highlights)
        where correctTokens = splitIntoWords correctAnswer
              studentTokens = splitIntoWords studentAnswer
              results = snd $ getResults dictWords (0, []) correctTokens studentTokens 0
              highlights = findRelevantHighlights results
