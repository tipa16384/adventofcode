import System.IO ()
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Bifunctor (second)

main :: IO ()
main = do
    lines <- readLinesFromFile "puzzle2.dat"
    putStrLn $ "Part 1: " ++ show (sum $ map fst $ filter snd $ map part1 lines)
    putStrLn $ "Part 2: " ++ show (sum $ map part2 lines)

data Color = Red | Blue | Green deriving (Eq, Show, Read)
type BallCount = (Color, Int)

parseColor :: String -> Color
parseColor str = case str of
    "red"   -> Red
    "blue"  -> Blue
    "green" -> Green

balls :: [BallCount]
balls = [(Red, 12), (Blue, 14), (Green, 13)]

-- function takes a line and returns a tuple of the game number and a boolean
-- representing whether the game is valid or not
part1 :: String -> (Int, Bool)
part1 line = do
    let parsedLine = parseLine line
    -- valid if every checkColorCount x in the ball reveals is True
    second (foldl (\acc x -> acc && checkColorCount x) True) parsedLine

-- function takes a line and returns the product of the maximum count of each color
part2 :: String -> Int
part2 line = do
    let reveals = snd $ parseLine line
        colors = nub $ map fst reveals
    product $ map (\x -> maximum $ map snd $ filter (\y -> fst y == x) reveals) colors

-- function takes a file path and returns a list of lines from the file
readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-- function takes a line and returns a tuple of the game number and a list of
-- (color, count) tuples
parseLine :: String -> (Int, [BallCount])
parseLine line = do
    let gameRevealsSplit = splitOn ": " line
        revealsSplit = splitOn "; " (gameRevealsSplit !! 1)
        reveals = concatMap (splitOn ", ") revealsSplit
        gameNumber = read (splitOn " " (head gameRevealsSplit) !! 1):: Int
    (gameNumber, map parseCountColor reveals)

-- function takes a string of the form "count color" and returns a (color, count) tuple
parseCountColor :: String -> BallCount
parseCountColor reveal = do
    let revealSplit = splitOn " " reveal
    (parseColor $ revealSplit !! 1, read (head revealSplit):: Int)

-- function takes a (color, count) tuple and checks if the count is valid
-- (less than or equal to the number of balls of that color)
checkColorCount :: BallCount -> Bool
checkColorCount colorCombo = snd (head $ filter (\x -> fst x == fst colorCombo) balls) >= snd colorCombo
