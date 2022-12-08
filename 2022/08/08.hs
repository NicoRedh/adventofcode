import Data.Char (digitToInt)
import Data.List (transpose)

data Tree = Hidden Int | Visible Int deriving (Show, Eq)
type  View = (Int, Int, Int, Int, Int) -- height left right up down

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

readViews :: [Char] -> [View]
readViews xs = [(digitToInt x, 0, 0, 0, 0) | x <- xs]

parseViews :: [Char] -> [[View]]
parseViews xs = map readViews $ lines xs

viewValue :: View -> Int
viewValue (h, l, r, o, u) = l*r*o*u

--beware: very dirty code below

determineRowView :: Int -> [View] -> [View]
determineRowView pos vs
  | pos == length vs = vs
  | otherwise = determineRowView (pos + 1) rs
  where rs = fh ++ [determineTreeView (vs !! pos)] ++ lh
        fh = take (pos) vs
        lh = drop (pos + 1) vs
        determineDist h ((he, le, re, oe, ue):ls)
          | ls == [] = []
          | h > he = (he,le,re,oe,ue) : determineDist h ls
          | otherwise =  []
        determineTreeView (h, l, r, o, u) = (h, if fh == [] then 0 else (length $ determineDist h $ reverse fh) + 1, if lh == [] then 0 else (length $ determineDist h lh) + 1, o, u)

determineColView :: Int -> [View] -> [View]
determineColView pos vs
  | pos == length vs = vs
  | otherwise = determineColView (pos + 1) rs
  where rs = fh ++ [determineTreeView (vs !! pos)] ++ lh
        fh = take (pos) vs
        lh = drop (pos + 1) vs
        determineDist h ((he, le, re, oe, ue):ls)
          | ls == [] = []
          | h > he = (he,le,re,oe,ue) : determineDist h ls
          | otherwise =  []
        determineTreeView (h, l, r, o, u) =(h, l, r, if fh == [] then 0 else (length $ determineDist h $ reverse fh) + 1, if lh == [] then 0 else (length $ determineDist h lh) + 1)

calcAllViews :: [[View]] -> [[Int]]
calcAllViews xs = map (map viewValue) xs

findMaxView :: [[View]] -> Int
findMaxView xs = maximum $  map maximum $ calcAllViews xs

solve xs = countVisible $ map rowAndReverse $  transpose $ map rowAndReverse $ parse xs

solve2 xs = findMaxView $ map (determineColView 0) $ transpose $ map (determineRowView 0) (parseViews xs)


main = do
  input <- getContents
  print $ solve input
  print $ solve2 input
