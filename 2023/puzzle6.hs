quadraticFormula :: Floating a => a -> a -> a -> (a, a)
quadraticFormula a b c = ((-b - sqrt d) / (2 * a), (-b + sqrt d) / (2 * a))
  where
    d = b**2 - 4*a*c

qds1 = map (quadraticFormula 1 . negate . fst) [(52, 426), (94, 1374), (75, 1279), (94, 1216)]
part1 = product [snd qd - fst qd | qd <- qds1]

qds2 = map (quadraticFormula 1 . negate . fst) [(52947594, 426137412791216)]
part2 = product [snd qd - fst qd | qd <- qds2]

main :: IO ()
main = do
  print ("Part 1:", part1)
  print ("Part 2:", part2)
