import Data.List (transpose)


applyOp :: ([Char], [Int]) -> Int
applyOp (ops, xs) = foldr (fst op) (snd op) xs
  where op = if ops == "+" then ((+), 0) else ((*), 1)

solve :: [Char] -> Int
solve xs = foldr (+) 0 (map applyOp (zip ops elems)) 
  where input = map words (lines xs) -- [[[Char]]]
        elems = transpose $ map (\x ->map read x) (init input) -- [[Int]]
        ops = last input


-- Part 2:

parse :: [[Char]] -> [[Char]] -> [[Char]]
parse ls xs
  | null (head xs) = ls
  | null ls = parse [h ++ [' ']] (txs)
  | all (== ' ') h = parse (ls ++ [ht ++ [' ']]) (ttxs)
  | otherwise = parse ((init ls) ++ [last ls ++ h ++ [' ']]) (txs)
  where h = foldr ((:) . head) [] xs
        ht = foldr ((:) . head) [] (txs)
        txs = foldr ((:) . tail) [] xs
        ttxs =foldr ((:) . tail) [] txs

solve2 xs = foldr (+) 0 (map applyOp (zip ops elems)) 
  where input = lines xs
        elems = map (map (\a -> read a::Int) . words) $ parse [] (init input)
        ops = words $ last input

main = do
  input <- getContents
  print $ "Part 1: " ++ show (solve input)
  print $ "Part 2: " ++ show (solve2 input)
