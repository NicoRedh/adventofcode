import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.List as L
type Range = (Int, Int)
  
readNums :: [Char] -> ([[Int]], [Int])
readNums xs =  (ranges, ingredients)
  where ranges =  map (map (read . T.unpack) . T.splitOn (T.pack "-")) (head pairOfRangesAndIngredients)
        ingredients =  map (read . T.unpack) (last pairOfRangesAndIngredients) --
        pairOfRangesAndIngredients = map T.words $ T.splitOn (T.pack ("\n\n")) $ T.pack xs -- [[Text]]

filterIngredients s ([], ing) = s
filterIngredients s (((x:y:[]):xs), ingredients) = filterIngredients newSet (xs, ingredients)
  where newSet = S.union s (S.fromList (filter (\ing -> x <= ing && ing <= y) ingredients))

solve xs = S.size $ filterIngredients S.empty $ readNums xs

-- Part 2:
readNums2 :: [Char] -> [[Int]]
readNums2 xs =  ranges
  where ranges =  map (map (read . T.unpack) . T.splitOn (T.pack "-")) (head pairOfRangesAndIngredients)
        pairOfRangesAndIngredients = map T.words $ T.splitOn (T.pack ("\n\n")) $ T.pack xs -- [[Text]]

--takes a sorted list of ranges and returns the count of all unique elements.
getRanges count [] = count
getRanges count ((x:y:[]):[]) = count + (y - x + 1)
getRanges count ((x:y:[]):(a:b:[]):xs)
  | y < a = getRanges (count + (y - x + 1)) ((a:b:[]):xs)
  | otherwise = getRanges count ((x:(max y b):[]):xs)

solve2 xs = getRanges 0 $ L.sortOn head $ readNums2 xs

main = do
  input <- getContents
  print $ "Part 1: " ++ show (solve input)
  print $ "Part 2: " ++ show (solve2 input)
