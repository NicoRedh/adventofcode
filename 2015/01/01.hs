repl x
  | x == '(' = 1
  | x == ')' = -1
  | otherwise = 0

solve xs = sum $ map repl xs
solve2 :: [Char] -> [Char] -> Int
solve2 xs (y:ys)
  | solve xs < 0 = length xs
  | otherwise    = solve2 (xs++[y]) ys
main = do
  input <- getLine
  print $ "Part1: " ++ show (solve input)
  print $ "Part2: " ++ show (solve2 [] input)