import Data.Bits
import Text.Printf (printf)
import Data.Text (splitOn, pack, unpack)
import qualified Data.Set as S (Set, fromList, member, empty, insert, foldr, union, map, filter, size)
import qualified Data.Map as M (Map, empty, insertWith, lookup, insert, foldl)
import Debug.Trace (trace)

parse :: [Char] -> (Int, S.Set Int)
parse xs = (mask, S.fromList bts)
  where wds = words xs
        mask = binToInt (map (\a -> if a == '.' then 0 else 1) ((init . tail . head) wds))
        sizeOfMask = length ((init . tail . head) wds)
        bts = (map (foldr (\cur acc -> (bit cur) .|. acc) 0 . map (read . unpack) . splitOn (pack ",") . pack . init . tail) . init . tail) wds
        binToInt bin = foldr (\curr acc -> curr + (acc * 2)) 0 bin
        
solveMachine :: Int -> Int -> S.Set Int -> S.Set Int -> Int
solveMachine n machine curMasks baseMasks
  | S.member machine curMasks = n
  | otherwise = solveMachine (n+1) machine newCurMasks baseMasks
    where newCurMasks = S.foldr (\cur acc -> S.union (S.map (xor cur) baseMasks) acc) S.empty curMasks

solve xs = sum (map (\a -> solveMachine 1 (fst a) (snd a) (snd a)) machines)
  where machines = map (parse) (lines xs)

-- Part 2:

parse2 :: [Char] -> (S.Set [Int], [Int])
parse2 xs = (S.fromList bts, jlt)
  where wds = words xs
        sizeOfMask = length ((init . tail . head) wds)
        bts = (map (map (read . unpack) . splitOn (pack ",") . pack . init . tail) . init . tail) wds
        --- -> [[Int]]
        --jltMap = foldr (\cur acc -> M.insert (fst cur) (snd cur) acc) M.empty (zip [0..] jlt)
        jlt = (map (read . unpack) . splitOn (pack ",") . pack . init . tail . last) wds
        
solveMachine2 :: Int -> [Int] -> S.Set [Int] -> S.Set [Int] -> Int
solveMachine2 n machine curMasks baseMasks | trace ("n: " ++ show n ++ "\t Length of curMasks: " ++  show (S.size curMasks)) False = undefined
solveMachine2 n machine curMasks baseMasks
  | S.member machine curMasks = n
  | otherwise = solveMachine2 (n+1) machine filterMasks baseMasks
    where newCurMasks = S.foldr (\cur acc -> S.union (S.map (addTo cur) baseMasks) acc) S.empty curMasks
          filterMasks = S.filter (isUnder machine) newCurMasks

isUnder :: [Int] -> [Int] -> Bool
isUnder [] [] = True
isUnder xs [] = True
isUnder (x:xs) (y:ys) = if y > x then False else isUnder xs ys

convert :: [Int] -> [Int]
convert xs | trace("Convert input: " ++ show xs) False = undefined
convert xs = [if elem a xs then 1 else 0 | a <- [0..(maximum xs)]] 

addTo :: [Int] -> [Int] -> [Int]
addTo [] ys = ys
addTo xs [] = xs
addTo (x:xs) (y:ys) = (x+y): addTo xs ys

iterMachines :: [(S.Set [Int],[Int])] -> Int
iterMachines xs | trace ("Remaining machines: " ++ show (length xs)) False = undefined
iterMachines [] = 0
iterMachines ((btns, jlts):ms) = (solveMachine2 1 jlts (S.map convert btns) (S.map convert btns)) + iterMachines ms
            
solve2 xs = iterMachines (map (parse2) (lines xs))

-- unfortunately part 2 seems to require linear algebra functionality which would rely on external libraries which I do not want to use.
-- The above approach would work and might even be optimized but I did realize at some point that this is just minimizing the solution to a
-- linear equation, which I did not have the time for
  
main = do
 input <- getContents
 printf "Part 1: %d\n" (solve input)
 printf "Part 2: %d\n" (solve2 input)
