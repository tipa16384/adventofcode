import System.IO

main :: IO ()
main = do
  lines <- readLinesFromFile "puzzle3.dat"
  let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
  putStrLn $ "Part 1: " ++ show (countTrees lines $ slopes !! 1)
  putStrLn $ "Part 2: " ++ show (product $ map (countTrees lines) slopes)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
  contents <- readFile filePath
  return (lines contents)

countTrees :: [String] -> (Int, Int) -> Int
countTrees lines (dx, dy) = do
  let ylist = [0,dy..((length lines) - 1)]
  let xlist = map (`mod` length (head lines)) [0,dx..]
  let coords = zip xlist ylist
  let trees = filter (\(x, y) -> (lines !! y) !! x == '#') coords
  length trees
