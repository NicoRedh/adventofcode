import Data.Set as Set

solve :: Num a => [[a]] -> [Char] -> [[a]]
solve [] xs     = solve [[0, 0]] xs
solve xs (y:ys)
  | y == '<' = solve (xs ++ [[head ls - 1, last ls]]) ys
  | y == '>' = solve (xs ++ [[head ls + 1, last ls]]) ys
  | y == 'v' = solve (xs ++ [[head ls, last ls - 1]]) ys
  | y == '^' = solve (xs ++ [[head ls, last ls + 1]]) ys
  | otherwise = solve xs ys
  where ls = last xs
solve xs _ = xs

evens (x:xs) = x:odds xs
evens [] = []

odds (_:xs) = evens xs
odds _ = []
solve2 xs = solve [[0,0]] ft ++ solve [[0,0]] sn
  where ft = evens xs
        sn = odds xs

main :: IO ()
main = do
    input <- getLine
    putStrLn ("Part1: " ++ show (length $Set.fromList $ solve [[0,0]] input))
    putStrLn ("Part2: " ++ show (length $Set.fromList $ solve2 input))
