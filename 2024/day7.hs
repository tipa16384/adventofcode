import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import System.IO (readFile)
import Text.Read (readMaybe)
import System.Clock (Clock(Monotonic), getTime, diffTimeSpec, toNanoSecs)

-- Evaluate recursively whether a combination of operations can produce the target
evaluateRecursive :: [Int] -> Int -> Int -> Bool
evaluateRecursive [] target current = current == target
evaluateRecursive (x:xs) target current =
  or [ evaluateRecursive xs target (current + x)
     , evaluateRecursive xs target (current * x)
     , if current /= 0 then evaluateRecursive xs target (read (show current ++ show x) :: Int) else False
     ]

-- Process a single line to determine if it matches the target
processLine :: String -> Int
processLine line =
  case words (map replaceColonWithSpace line) of
    (targetStr:rest) ->
      let target = read targetStr :: Int
          numbers = map read rest
       in if evaluateRecursive numbers target 0 then target else 0
    _ -> 0
  where
    replaceColonWithSpace ':' = ' '
    replaceColonWithSpace c = c

-- Calculate the total calibration result
calculateTotalCalibration :: [String] -> Int
calculateTotalCalibration lines =
  sum results
  where
    results = map processLine lines `using` parList rdeepseq

-- Main function

main :: IO ()
main = do
  start <- getTime Monotonic
  contents <- readFile "in.txt"
  let inputLines = filter (not . null) (lines contents)
  let totalCalibration = calculateTotalCalibration inputLines
  print $ "Total Calibration Result: " ++ show totalCalibration
  end <- getTime Monotonic
  let duration = fromIntegral (toNanoSecs (diffTimeSpec start end)) / 1e6
  putStrLn $ "Execution Time: " ++ show duration ++ "ms"
