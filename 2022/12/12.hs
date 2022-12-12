import Data.Map (Map, insert, empty, union, filter, keys, (!), findMax, size, member, unions, lookup)
import Debug.Trace (trace)
import Data.Maybe (isJust)

type Grid = Map (Int,Int) Int

parse :: [Char] -> Grid
parse xs = putLines 0 (lines xs) empty
    where putLines :: Int -> [[Char]] -> Grid -> Grid
          putLines n xs map
            | length xs == n = map
            | otherwise = putLines (n+1) xs (putLine 0 n (xs !! n) map)
          putLine :: Int -> Int -> [Char] -> Grid -> Grid
          putLine x y xs map
            | length xs == x = map
            | xs !! x == 'S' = putLine (x+1) y xs (insert (x,y) 0 map)
            | xs !! x == 'E' = putLine (x+1) y xs (insert (x,y) 27 map)
            | otherwise = putLine (x+1) y xs (insert (x,y) (fromEnum  (xs !! x) - 96) map)

findEnd xs = head $ keys $ Data.Map.filter ( == 27) xs

adjustEnd xs = insert (findEnd xs) 26 xs

determineDist :: (Int,Int) -> Int -> Grid -> Grid -> Grid
--determineDist (maxx, maxy) n heightMap distMap | trace ("n: " ++ show n ++ " size of DistMap: " ++ show (size distMap))  False = undefined
determineDist (maxx, maxy) n heightMap distMap
    | mod n (div (size heightMap) 10) == 0 = if distMap == newDistMap 0 distMap then distMap else determineDist (maxx,maxy) (n+1) heightMap (newDistMap 0 distMap)
    | x > maxx || y > maxy = distMap
    | otherwise = determineDist (maxx,maxy) (n+1) heightMap (newDistMap 0 distMap)
    where x = mod n (maxx + 1)
          y = div n (maxx + 1)
          newDistMap m dMap
            | m > n = dMap
            | otherwise = newDistMap (m + 1) $ mapDist (keys (Data.Map.filter (== m) dMap)) `union` dMap
            where   mapDist :: [(Int, Int)] -> Grid
                    mapDist ks = foldl checkAll empty ks
                    checkAll :: Grid -> (Int, Int) -> Grid
                    checkAll g k@(keyx, keyy) = checkD (checkU (checkL (checkR g k) k) k) k
                    checkL g k@(keyx, keyy) = if keyx > 0    && heightMap ! (keyx, keyy) + 1 >= heightMap ! (keyx - 1, keyy) then if member (keyx - 1, keyy) dMap then if dMap ! (keyx - 1, keyy) > m + 1 then insert (keyx - 1, keyy) (m + 1) g else g else insert (keyx - 1, keyy) (m + 1) g else g
                    checkR g k@(keyx, keyy) = if keyx < maxx && heightMap ! (keyx, keyy) + 1 >= heightMap ! (keyx + 1, keyy) then if member (keyx + 1, keyy) dMap then if dMap ! (keyx + 1, keyy) > m + 1 then insert (keyx + 1, keyy) (m + 1) g else g else insert (keyx + 1, keyy) (m + 1) g else g
                    checkD g k@(keyx, keyy) = if keyy > 0    && heightMap ! (keyx, keyy) + 1 >= heightMap ! (keyx, keyy - 1) then if member (keyx, keyy - 1) dMap then if dMap ! (keyx, keyy - 1) > m + 1 then insert (keyx, keyy - 1) (m + 1) g else g else insert (keyx, keyy - 1) (m + 1) g else g
                    checkU g k@(keyx, keyy) = if keyy < maxy && heightMap ! (keyx, keyy) + 1 >= heightMap ! (keyx, keyy + 1) then if member (keyx, keyy + 1) dMap then if dMap ! (keyx, keyy + 1) > m + 1 then insert (keyx, keyy + 1) (m + 1) g else g else insert (keyx, keyy + 1) (m + 1) g else g




solve xs = Data.Map.lookup end $ determineDist (maximum $ keys adjustedInput) 0 adjustedInput (insert (head $ keys $ Data.Map.filter (== 0) adjustedInput) 0 empty)
    where adjustedInput = adjustEnd input
          input = parse xs
          end = findEnd input

solve2 :: [Char] -> Maybe Int
solve2 xs = minimum $ Prelude.filter isJust [Data.Map.lookup end $ determineDist (maximum $ keys adjustedInput) 0 adjustedInput (insert x 0 empty) | x <- keys $ Data.Map.filter (<= 1) adjustedInput]
    where adjustedInput = adjustEnd input
          input = parse xs
          end = findEnd input

main = do
    input <- getContents
    print $ solve input
    print $ solve2 input
