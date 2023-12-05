import System.IO ()
import Data.List (group, sort)


main :: IO ()
main = do
    lines <- readLinesFromFile "puzzle4.dat"
    let duplicates = map getDuplicates lines
    putStrLn $ "Part 1: " ++ show (sum $ map (\x -> 2 ^ (x-1)) (filter (> 0) duplicates))
    let part2Sum = sum $ map (\x ->  1 + playGame (drop x duplicates)) [0..length duplicates - 1]
    putStrLn $ "Part 2: " ++ show part2Sum

-- function takes a list of numbers and returns the sum of the game
playGame :: [Int] -> Int
-- if the list is empty, return 0
playGame [] = 0
-- if head is 0, return 0
playGame (0:xs) = 0
-- otherwise (x:xs) call sum playGame called with the next x elements of xs and add x
playGame (x:xs) = x + sum (map (\z -> playGame $ drop z xs) [0..x-1])

-- function takes a string, splits it into words, and returns a list of words that appear more than once
-- (aside from the first two that encode the game number)
getDuplicates :: String -> Int
getDuplicates str = length $ filter (\x -> length x > 1) (group (sort (drop 2 $ words str)))

-- function takes a file path and returns a list of lines from the file
readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)
