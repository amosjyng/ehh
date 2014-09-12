import System.IO
import System.Environment
import Data.List.Split

main = do
        args <- getArgs
        let correctAnswer = head args
        let studentAnswer = last args
        putStrLn $ splitOn " " correctAnswer !! 0