import Data.Char (digitToInt)
import Data.List (transpose)

data Tree = Hidden Int | Visible Int deriving (Show, Eq)

readTrees :: [Char] -> [Tree]
readTrees xs = [Hidden (digitToInt x) | x <- xs]

determineRow :: [Tree] -> Int -> [Tree]
determineRow ((Visible num):trees) maxi
  | maxi == 9 = (Visible num) : trees
  | otherwise = (Visible num) : determineRow trees (maximum [num, maxi])
determineRow ((Hidden num):trees) maxi
  | maxi == 9 = (Hidden num) : trees
  | num > maxi = (Visible num) : determineRow trees num
  | otherwise = (Hidden num) : determineRow trees maxi
determineRow [] _ = []

rowAndReverse :: [Tree] -> [Tree]
rowAndReverse xs = determineRow (reverse $ determineRow xs (-1)) (-1)

countVisibleRow :: [Tree] -> Int
countVisibleRow [] = 0
countVisibleRow ((Visible num):xs) = 1 + countVisibleRow xs
countVisibleRow ((Hidden num):xs) = 0 + countVisibleRow xs

countVisible :: [[Tree]] -> Int
countVisible xs = sum $ map countVisibleRow xs

parse :: [Char] -> [[Tree]]
parse xs = map readTrees $ lines xs
--solve :: [Char] -> [[Int]]
solve xs = countVisible $ map rowAndReverse $  transpose $ map rowAndReverse $ parse xs
main = do
  input <- getContents
  print $ solve input
