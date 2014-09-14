module Main where

import System.Environment
import Grade

-- | From commandline arguments, see if the student answer reasonably matches
-- the expected correct answer.
--
-- Usage: grade <dictionary file> <correct answer> <student answer>
main :: IO ()
main = do
        [filename, correctString, studentString] <- getArgs
        dict <- readFile filename
        print $ grade (map toLowerStr $ lines dict) correctString studentString
