-- Part1:
calc :: [Int] -> (Int, Int) -> Int
calc [] (value, count) = count
calc (x:xs) (value, count) = calc xs (newVal, (if newVal == 0 then count + 1 else count))
  where newVal = mod (value + x) 100

signage :: [Char] -> Int
signage (x:xs)
  | x == 'R' = (read xs)
  | otherwise = -(read xs)
  
solve :: [Char] -> Int
solve xs = calc (map signage (words xs)) (50, 0)

-- Part2:
calc2 :: [Int] -> (Int, Int) -> Int
calc2 [] (value, count) = count
calc2 (x:xs) (value, count) = calc2 xs (newVal, (count + abs (div (value + x) 100)))
  where newVal = mod (value + x) 100
                        
solve2 :: [Char] -> Int
solve2 xs = calc2 (map signage (words xs)) (50, 0)


main = do
  input <- getContents
  print $ "Part1: " ++ show (solve input)
  print $ "Part2: " ++ show (solve2 input)

