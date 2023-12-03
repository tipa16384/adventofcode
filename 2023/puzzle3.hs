import System.IO ()
import Data.Char (isDigit)

main :: IO ()
main = do
    lines <- readLinesFromFile "puzzle3.dat"
    let numbers = concatMap (uncurry parseNumbers) (zip [0..] lines)
        partNumbersTuples = filter (\(row, col, len, num) -> checkSurrounding (row, col, len, num) lines) numbers
        partNumbers = map (\(row, col, len, num) -> num) partNumbersTuples
    putStrLn $ "Part 1: " ++ show (sum partNumbers)

-- given a list of lines, return (row, col) coordinates of every '*'
getAsteriskCoords :: [String] -> [(Int, Int)]
getAsteriskCoords lines = do
    let height = length lines
        width = length (head lines)
    [(r, c) | r <- [0..height - 1], c <- [0..width - 1], lines !! r !! c == '*']

-- given a coordinate, return a list of number tuples that have that coordinate in checkSurrounding
getNumbersWithCoord :: (Int, Int) -> [(Int, Int, Int, Int)] -> [(Int, Int, Int, Int)]
getNumbersWithCoord (row, col) numbers = filter (\(r, c, len, num) -> (row, col) `elem` getSurroundingCoords (r, c, len, num) (length numbers, length numbers)) numbers

-- function that takes a (row, col, len, num) tuple and a list of lines and calls
-- getSurroundingCoords to get the surrounding coordinates of the number
-- and returns true if any of the surrounding characters are not digits and are not '.'
checkSurrounding :: (Int, Int, Int, Int) -> [String] -> Bool
checkSurrounding (row, col, len, num) lines = do
    let surroundingCoords = getSurroundingCoords (row, col, len, num) (length lines, length (head lines))
        surroundingChars = map (\(r, c) -> lines !! r !! c) surroundingCoords
    any (\x -> not (isDigit x) && x /= '.') surroundingChars

-- function takes a file path and returns a list of lines from the file
readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-- Function to get surrounding coordinates
getSurroundingCoords :: (Int, Int, Int, Int) -> (Int, Int) -> [(Int, Int)]
getSurroundingCoords (row, col, len, num) (height, width) =
    filter withinBounds allSurroundingCoords
  where
    -- Calculate end column of the number
    endCol = col + len - 1

    -- All possible surrounding coordinates
    allSurroundingCoords = [(r, c) | r <- [row - 1, row, row + 1],
                                     c <- [col - 1, col..endCol + 1],
                                     not (r == row && c >= col && c <= endCol)]

    -- Check if a coordinate is within the grid boundaries
    withinBounds (r, c) = r >= 0 && r < height && c >= 0 && c < width


-- Main function to process the string, now takes an additional row number
parseNumbers :: Int -> String -> [(Int, Int, Int, Int)]
parseNumbers rowNum str = parseHelper str 0
  where
    parseHelper :: String -> Int -> [(Int, Int, Int, Int)]
    parseHelper [] _ = []
    parseHelper (x:xs) idx
      | isDigit x = let (numStr, rest) = span isDigit (x:xs)
                        num = read numStr
                        len = length numStr
                    in (rowNum, idx, len, num) : parseHelper rest (idx + len)
      | otherwise = parseHelper xs (idx + 1)
