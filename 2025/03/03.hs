import Data.List
import Data.Ord
-- Part 1:
getJoltage :: (Char, Char) -> [Char] -> Int
getJoltage (f, s) [] = read [f,s] :: Int
getJoltage (f, s) (x:[])
  | s < x= getJoltage (f, x) []
  | otherwise = getJoltage (f, s) []
getJoltage (f, s) (x:y:xs)
  | f < x = getJoltage (x, '0') (y:xs)
  | s < x = getJoltage (f, x) (y:xs)
  | otherwise = getJoltage (f, s) (y:xs)

solve xs = sum $ map (getJoltage ('0', '0')) $ words xs

-- Part 2:

maxWIndex :: (Ord a1, Ord a2, Num a2, Enum a2) => [a1] -> (a2, a1)
maxWIndex = maximumBy (comparing snd <> flip (comparing fst)) . zip [0..]

getMaxJoltage :: Int -> [Char] -> [Char]
getMaxJoltage 0 xs = []
getMaxJoltage 1 xs = [snd (maxWIndex xs)]
getMaxJoltage digits xs = (snd res) : getMaxJoltage (digits - 1) (drop (fst res + 1) xs)
  where res = maxWIndex (take (length xs - digits + 1) xs)

solve2 xs = sum $ (map read $ (map (getMaxJoltage 12) (words xs)))


main = do
  input <- getContents
  print $ "Part 1: " ++ show (solve input)
  print $ "Part 2: " ++ show (solve2 input)
  
