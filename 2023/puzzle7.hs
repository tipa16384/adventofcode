
readPuzzle :: IO [(String, Int)]
readPuzzle = do
    contents <- readFile "puzzle7.dat"
    return [(w1, read w2) | line <- lines contents, let (w1:w2:_) = words line]

score :: [[a]] -> String -> String -> String
score groups cardValues hand = value ++ map (translateCard cardValues) hand
  where
    value = show (5 + length (head groups) - length groups)
    translateCard :: String -> Char -> Char
    translateCard values c = "0123456789ABC" !! indexInCardValues values c

    indexInCardValues :: String -> Char -> Int
    indexInCardValues values c = case c of
        'T' -> indexOf 'A' values
        'J' -> indexOf 'B' values
        'Q' -> indexOf 'C' values
        'K' -> indexOf 'D' values
        'A' -> indexOf 'E' values
        _   -> indexOf c values

    indexOf :: Char -> String -> Int
    indexOf c values = case elemIndex c values of
      Just n  -> n
      Nothing -> error "Card not found in card values"
