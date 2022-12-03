import qualified Data.Char as C
findCommon :: [Char] -> [Char]
findCommon xs =
  let half = div (length xs) 2
  in [head [x | x <- (take half xs), elem x (drop half xs)]]

splitInGroupsOfN :: Int -> [[Char]] -> [[[Char]]]
splitInGroupsOfN _ [] = []
splitInGroupsOfN n xs = (take n xs) : (splitInGroupsOfN n $ drop n xs)

findBadge :: [[[Char]]] -> [[Char]]
findBadge xs = map common xs
  where common xs = [head [ x | x <- head xs, elem x (xs !! 1) && elem x (xs !! 2)]]

mapToNum :: [Char] -> [Int]
mapToNum xs = map conv xs
  where conv x = if C.isUpper x then (fromEnum x) - 38 else (fromEnum x) - 96

solve xs = sum $ map (sum . mapToNum . findCommon) $ lines xs

solve2 xs = sum $ map (sum .  mapToNum) $ findBadge $ splitInGroupsOfN 3 $ lines xs



main = do
  input <- getContents
  print $ solve input
  print $ solve2 input

