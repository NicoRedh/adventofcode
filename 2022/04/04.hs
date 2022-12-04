import qualified Data.Text as T
readNums :: [Char] -> [Int]
readNums xs =  map (read . T.unpack) ls
  where ls = concat $ map (T.splitOn (T.pack "-")) $ T.splitOn (T.pack ",") $ T.pack xs

parseLines :: [[Char]] -> [[Int]]
parseLines xs = map readNums xs

contains :: [Int] -> Int
contains [a,b,c,d] = if (a >= c && b <= d) || (a <= c && b >= d) then 1 else 0

contains2 :: [Int] -> Int
contains2 [a,b,c,d] = if (b >= c && a <= d) || (b <= c && a >= d)  then 1 else 0

solve xs =sum $ map contains $  parseLines $ lines xs
solve2 xs = sum $ map contains2 $  parseLines $ lines xs

main = do
  input <- getContents
  print $ solve input
  print $ solve2 input
