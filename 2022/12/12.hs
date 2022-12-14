import Data.Map (Map, insert, empty, (!), lookup, toList, delete)
import Debug.Trace (trace)
import Data.Foldable (minimumBy)
import Data.Function (on)

type Grid = Map (Int,Int) Int
type Coord = (Int, Int)

--           pos    neighbors height visited (start, end, middle) dist
type Elem = (Coord, [Coord], Int, Int, Int)
type Elemap = Map Coord [Coord]
type DistMap = Map Coord Int

-- int here is the part no, because the starting options change
parse :: [Char] -> Int -> [[Elem]]
parse xs part = putLines 0 (lines xs)
    where putLines :: Int -> [[Char]] -> [[Elem]]
          putLines n xs
            | length xs == n = []
            | otherwise = putLine 0 n (xs !! n) : putLines (n+1) xs
          putLine :: Int -> Int -> [Char] -> [Elem]
          putLine x y xs
            | length xs == x = []
            | xs !! x == 'S' = ((x,y), [], 1, 1, 0) : putLine (x+1) y xs
            | xs !! x == 'E' = ((x,y), [], 26, 2, maxBound) : putLine (x+1) y xs
            | xs !! x == 'a' = ((x,y), [], 1, 1, if part == 2 then 0 else maxBound) : putLine (x+1) y xs
            | otherwise = ((x,y), [], fromEnum (xs !! x) - 96, 3, maxBound) : putLine (x+1) y xs

getElem :: Coord -> [[Elem]] -> Elem
--getElem (x,y) ls | trace ("x: " ++ show x ++ " y: " ++ show y) False = undefined
getElem (x,y) ls = (ls !! y) !! x

getNeighbors :: [[Elem]] -> [[Elem]]
getNeighbors xs = map (map detN) xs
    where detN ((x,y), ns, height, ps, ds) = ((x,y),
                                             (if x < maxx then checkR (getElem (x + 1, y) xs) else []) ++
                                             (if x > 0    then checkL (getElem (x - 1, y) xs) else []) ++
                                             (if y < maxy then checkD (getElem (x, y + 1) xs) else []) ++
                                             (if y > 0    then checkU (getElem (x, y - 1) xs) else []) ++ ns,
                                             height,
                                             ps,
                                             ds)
                where checkL neigh@((nx,ny), _, nh, _, _) = [(nx,ny) | height + 1 >= nh]
                      checkR neigh@((nx,ny), _, nh, _, _) = [(nx,ny) | height + 1 >= nh]
                      checkD neigh@((nx,ny), _, nh, _, _) = [(nx,ny) | height + 1 >= nh]
                      checkU neigh@((nx,ny), _, nh, _, _) = [(nx,ny) | height + 1 >= nh]
                      maxx = length (head xs) - 1
                      maxy = length xs - 1

determineDist :: Elemap -> DistMap -> DistMap -> DistMap
--determineDist elemap rest result | trace ("\nresultMap: " ++ show result) False = undefined
determineDist elemap rest result
  | null rest = result
  | otherwise = determineDist elemap newRest newResult
    where (currCoord,currDist) = minimumBy (compare `on` snd) (toList rest)
          restWithoutCurrMin = delete currCoord rest
          newRest = foldl inspectNeighbor restWithoutCurrMin (elemap ! currCoord)
                    where --inspectNeighbor mapMaybeWithNeighbors neighbor | trace ("\nmap to be searched: " ++ show mapMaybeWithNeighbors ++ "\nneighbor that is examined: " ++ show neighbor) False = undefined
                          inspectNeighbor mapMaybeWithNeighbors neighbor = case Data.Map.lookup neighbor restWithoutCurrMin of Nothing -> mapMaybeWithNeighbors
                                                                                                                               Just nds -> if nds > currDist + 1 then insert neighbor (currDist + 1) mapMaybeWithNeighbors else mapMaybeWithNeighbors
          newResult = insert currCoord currDist result

elemToDistMap :: [[Elem]] -> DistMap -> DistMap
elemToDistMap [] elmap = elmap
elemToDistMap (fs:xs) elmap = elemToDistMap xs (elmapLine fs elmap)
    where elmapLine [] currElmap = currElmap
          elmapLine (((x,y), ns, height, ps, ds):xs) currElmap = elmapLine xs $ insert (x,y) ds currElmap

elemToElemap :: [[Elem]] -> Elemap -> Elemap
elemToElemap [] elmap = elmap
elemToElemap (fs:xs) elmap = elemToElemap xs (elmapLine fs elmap)
    where elmapLine [] currElmap = currElmap
          elmapLine (((x,y), ns, height, ps, ds):xs) currElmap = elmapLine xs $ insert (x,y) ns currElmap

findEndCoords :: [[Elem]] -> Coord
findEndCoords xs = getCoord $ head $ concatMap getEnd xs
  where getEnd xs = [x | x <- xs, getPos x == 2]
        getPos (_,_,_,ps,_) = ps
        getCoord (c,_,_,_,_) = c

solve1 xs = Data.Map.lookup endCoords $ determineDist elmap distMap empty
  where elems = getNeighbors (parse xs 1)
        distMap = elemToDistMap elems empty
        elmap = elemToElemap elems empty
        endCoords = findEndCoords elems
solve2 xs = Data.Map.lookup endCoords $ determineDist elmap distMap empty
  where elems = getNeighbors (parse xs 2)
        distMap = elemToDistMap elems empty
        elmap = elemToElemap elems empty
        endCoords = findEndCoords elems
main = do
    input <- getContents
    print $ solve1 input
    print $ solve2 input
