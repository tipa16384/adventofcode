import System.IO
import Data.List.Split (splitOn)

main :: IO ()
main = do
  lines <- readLinesFromFile "puzzle2.dat"
  let parsedLines = map parseLine (lines)
  print (length (filter isValid parsedLines))
  print (length (filter isValidPart2 parsedLines))

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
  contents <- readFile filePath
  return (lines contents)

parseLine :: String -> (Int, Int, Char, String)
parseLine line = (read a :: Int, read b :: Int, head c, d)
    where
        [ab, c, d] = words line
        [a, b] = splitOn "-" ab

isValid :: (Int, Int, Char, String) -> Bool
isValid (min, max, c, str) = do
  let count = length $ filter (== c) str in count >= min && count <= max

isValidPart2 :: (Int, Int, Char, String) -> Bool
isValidPart2 (pos1, pos2, c, str) = do
  let c1 = str !! (pos1 - 1)
  let c2 = str !! (pos2 - 1)
  (c1 == c && c2 /= c) || (c1 /= c && c2 == c)
