import qualified Data.Set as S
import qualified Data.Map as M

getSIndex :: [Char] -> Int
getSIndex (x:xs)
  | x == 'S' = 0
  | otherwise = 1 + getSIndex xs

getSplIndices :: Int -> [Char] -> [Int]
getSplIndices idx (x:xs)
  | null (xs) = []
  | x == '.' = getSplIndices (idx + 1) xs
  | otherwise = idx : getSplIndices (idx + 1) xs

getRays :: S.Set Int -> [[Char]] -> Int-- initialise with non-empty set
getRays rays (l:lines)
  | null lines = 0
  | otherwise = getDisFromLine rays spls + getRays (getRaysFromLine rays spls) lines
    where spls = getSplIndices 0 l

getRaysFromLine :: S.Set Int -> [Int] -> S.Set Int-- set is from row before
getRaysFromLine rays spls = S.foldr (\a -> if elem a spls then (S.insert (a - 1) . S.insert (a + 1)) else S.insert a) S.empty rays

getDisFromLine :: S.Set Int -> [Int] -> Int
getDisFromLine rays spls = S.size (S.difference (S.fromList spls) rays)

countTotalSplits xs = length (filter (=='^') xs)

solve xs = (countTotalSplits xs) -  getRays (S.fromList [getSIndex (head input)]) (tail input)
  where input = lines xs

-- Part 2:

getRays2 :: M.Map Int Int -> [[Char]] -> M.Map Int Int-- initialise with non-empty set
getRays2 rays (l:lines)
  | null lines = rays
  | otherwise =  getRays2 (getRaysFromLine2 rays spls) lines
    where spls = getSplIndices 0 l

getRaysFromLine2 :: M.Map Int Int -> [Int] -> M.Map Int Int
getRaysFromLine2 rays spls = M.foldrWithKey helper M.empty rays
  where helper key value acc
          | elem key spls = M.insertWith (+) (key + 1) (value) $ M.insertWith (+) (key - 1) (value) acc
          | otherwise  = M.insertWith (+) key value acc

getSum rays = M.foldrWithKey (\key val acc -> val + acc) 0 rays

solve2 xs = getSum $ getRays2 (M.insert (getSIndex (head input)) 1 M.empty) (tail input)
  where input = lines xs
main = do
  input <- getContents
  --print ""
  print $ "Part 1: " ++ show (solve input)
  print $ "Part 2: " ++ show (solve2 input)

