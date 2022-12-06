import Data.Set (fromList, size)


isMarker :: [Char] -> Bool
isMarker xs = length xs == size (fromList xs) -- O(nlogn)

solve :: [Char] -> Int
solve [] = 0
solve (x:xs)
  | isMarker (take 4 (x:xs)) = 4
  | otherwise = 1 + solve xs

solve2 :: [Char] -> Int
solve2 [] = 0
solve2 (x:xs)
  | isMarker (take 14 (x:xs)) = 14
  | otherwise = 1 + solve2 xs

main = do
  input <- getLine
  print $ solve input
  print $ solve2 input
