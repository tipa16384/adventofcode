import System.IO

part1 :: IO ()
part1 = do
    putStrLn "Enter a string of parentheses:"
    input <- getLine
    let result = processParentheses input
    print result

part2 :: IO ()
part2 = do
    putStrLn "Enter a string of parentheses:"
    input <- getLine
    let result = processParentheses2 input
    print result

processParentheses2 :: String -> Maybe Int
processParentheses2 = fst . foldl (\(mi, acc) 
    (i, x) -> if acc < 0 then (mi, acc) 
    else if x == '(' then (mi, acc + 1) else (Just i, acc - 1)) (Nothing, 0) . zip [1..]


processParentheses :: String -> Int
processParentheses = foldl (\acc x -> if x == '(' then acc + 1 else acc - 1) 0
