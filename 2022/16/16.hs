import Data.Map (Map, insert, keys, (!), empty, filter, lookup, toList, delete)
import Debug.Trace (trace)
import Data.List (minimumBy, maximumBy, permutations, sort)
import Data.Function (on)
import Control.Parallel
import Control.Monad

type Valve = (Int, [String], Bool, [Char])
type ValveMap = Map [Char] Valve
type DistMap = Map [Char] Int

parse :: [Char] -> ValveMap
parse xs = foldl parseLine empty (lines xs)
  where parseLine valveMap line = insert (wds !! 1) (read (init (drop 5 (wds !! 4))), map (take 2) (drop 9 wds), False, wds !! 1) valveMap
          where wds = words line

valveMapToDistMap :: Valve -> ValveMap -> DistMap
valveMapToDistMap start@(_,_,_,name) valveMap = insert name 0 $ foldl (\map key -> insert key maxBound map) empty (keys valveMap)

determineDist :: ValveMap -> DistMap -> DistMap -> DistMap
--determineDist elemap rest result | trace ("\nresultMap: " ++ show result) False = undefined
determineDist elemap rest result
  | null rest = result
  | otherwise = determineDist elemap newRest newResult
    where (currCoord,currDist) = minimumBy (compare `on` snd) (toList rest)
          restWithoutCurrMin = delete currCoord rest
          newRest = foldl inspectNeighbor restWithoutCurrMin (getNeighbors (elemap ! currCoord))
                    where --inspectNeighbor mapMaybeWithNeighbors neighbor | trace ("\nmap to be searched: " ++ show mapMaybeWithNeighbors ++ "\nneighbor that is examined: " ++ show neighbor) False = undefined
                          inspectNeighbor mapMaybeWithNeighbors neighbor = case Data.Map.lookup neighbor restWithoutCurrMin of Nothing -> mapMaybeWithNeighbors
                                                                                                                               Just nds -> if nds > currDist + 1 then insert neighbor (currDist + 1) mapMaybeWithNeighbors else mapMaybeWithNeighbors
          newResult = insert currCoord currDist result



getMaxRelief :: Int -> Valve -> ValveMap -> Int
--getMaxRelief minute curr@(currVal, currNeigh, currOpen, currName) valveMap | trace ("minute: " ++ show minute ++ "\ncurr: " ++ show curr ++ "\nallOpen: " ++ show (allOpen valveMap) ++ "\n") False = undefined
getMaxRelief minute curr@(currVal, currNeigh, currOpen, currName) valveMap
  | minute >= 31 = 0                                                                -- 30 minutes are over
  | allOpen valveMap = (29 - minute) *  sumOpen valveMap -- all Valves are open
  | otherwise = maximum [getMaxRelief (minute + (distMap ! nextValveKey) ) (valveMap ! nextValveKey) newValveMap + (sumOpen valveMap * (distMap ! nextValveKey) - 1)| nextValveKey <- keys distMap, isNotOpen (valveMap ! nextValveKey), isNotNull (valveMap ! nextValveKey)]
    where distMap = determineDist valveMap (valveMapToDistMap curr valveMap) empty
          newValveMap = open curr valveMap
          val valveKey = (30 - (1 + minute + (distMap ! valveKey))) * getValue (valveMap ! valveKey)

getNeighbors :: Valve -> [String]
getNeighbors (_,ns,_,_) = ns

getName :: Valve -> [Char]
getName (_,_,_,name) = name

getValue :: Valve -> Int
getValue (value, _, _,_) = value

isNotNull :: Valve -> Bool
isNotNull (a,_,_,_) = a > 0

isNotOpen :: Valve -> Bool
isNotOpen (_,_,b,_) = not b

sumOpen :: ValveMap -> Int
sumOpen valveMap = foldl addOpen 0 (keys $ Data.Map.filter (\(_, _, b,_) -> b) valveMap)
  where addOpen val key = val + getValue (valveMap ! key)

sumAll :: ValveMap -> Int
sumAll valveMap = foldl addOpen 0 (keys valveMap)
  where addOpen val key = val + getValue (valveMap ! key)

open :: Valve -> ValveMap -> ValveMap
open v@(a, b, _, name) = insert name (a,b,True,name)

allOpen :: ValveMap -> Bool
allOpen valveMap = sumOpen valveMap == sumAll valveMap

calcValOfList :: ValveMap -> [Valve] -> Int -> [Int]
--calcValOfList vMap (x:xs) n | trace ("minute: " ++ show n ++ " curr: " ++ show x ++ "\nlist: " ++ show (map getName (x:xs))) False = undefined
calcValOfList _ [] _= []
calcValOfList _ [x] n = [getValue x * (30 - n) | n < 30]
calcValOfList vMap (x:xs) n
  | n > 30 = []
  | otherwise = getValue x * (30 - n) : calcValOfList vMap xs ((determineDist vMap (valveMapToDistMap x vMap) empty ! (getName (head xs) )) + n + 1)

solve xs = getMaxRelief 2 (vMap ! "AA") vMap where vMap = parse xs
solve2 xs = maximum ([sum (calcValOfList (parse xs) perm 0) | perm <- map (\a -> parse xs ! "AA" : a) $ take 4000000 $ permutations (map snd (toList (Data.Map.filter isNotNull (parse xs)))) ] `par` [sum (calcValOfList (parse xs) perm 0) | perm <- map (\a -> parse xs ! "AA" : a) $ drop 4000000 $ permutations (map snd (toList (Data.Map.filter isNotNull (parse xs)))) ] )

main = do
  input <- getContents
  --print $ solve input
  print $ solve2 input
