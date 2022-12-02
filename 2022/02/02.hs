

solve xs = sum $ map calcPoints $ lines xs
solve2 xs = sum $ map calcPoints2 $ lines xs
calcPoints x
  | x == "A X" = 3 + 1
  | x == "A Y" = 6 + 2
  | x == "A Z" = 0 + 3
  | x == "B X" = 0 + 1
  | x == "B Y" = 3 + 2
  | x == "B Z" = 6 + 3
  | x == "C X" = 6 + 1
  | x == "C Y" = 0 + 2
  | x == "C Z" = 3 + 3
  | otherwise = 0

calcPoints2 x
  | x == "A X" = 0 + 3
  | x == "A Y" = 3 + 1
  | x == "A Z" = 6 + 2
  | x == "B X" = 0 + 1
  | x == "B Y" = 3 + 2
  | x == "B Z" = 6 + 3
  | x == "C X" = 0 + 2
  | x == "C Y" = 3 + 3
  | x == "C Z" = 6 + 1
  | otherwise = 0
  
main = do
  input <- getContents
  print $ solve input
  print $ solve2 input
