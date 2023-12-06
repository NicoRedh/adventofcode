-- disgusting code ahead!!

import Data.Char as C
import Data.List as L
numbers = [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]


replaceConc :: (String, String) -> String -> String
replaceConc xs (y:ys)
  | take len (fst xs) == take len (y:ys) = [y] ++ snd xs ++ys
  | otherwise = y : replaceConc xs ys
    where len = length $ fst xs

replace :: (String, String) -> String -> String
replace xs ys = if (fst xs) `L.isInfixOf` ys then replaceConc xs ys else ys 

deletter :: [(String, String)] -> String -> String
deletter [] xs = xs
deletter (n:nums) xs = deletter nums (replace n xs)

getFirst :: String -> Char
getFirst [] = '0'
getFirst (x:xs)
  | C.isDigit x = x
  | otherwise = getFirst xs

--getDigits :: String -> Integer  
getDigits xs = read $ getFirst xs : [getFirst (reverse xs)]

solve xs = sum (map getDigits (lines xs))

-- very disgusting solution to numbers appearing up to three times
-- should be handled better, if numbers appear four or more times
presolve xs = getDigits $ deletter numbers ( deletter numbers (deletter numbers xs))

solve1 xs = sum (map presolve (lines xs))

main = do
  input <- getContents
  print $ "Part1: " ++ show (solve input)
  print $ "Part2: " ++ show (solve1 input)
