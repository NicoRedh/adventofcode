import Data.Map (Map, insert, empty, size, filter, keys, member)
import Debug.Trace (trace)
import Data.List (delete)

data Elem = Sensor | Beacon | NoBeacon deriving (Eq) 
type Coord = (Int, Int)
type Elemap = Map Coord Elem
type Edgemap = Map Coord Bool

parse :: [Char] -> [(Coord, Coord)]
parse xs = map parseLine (lines xs)
    where parseLine xs = ((read $ init $ drop 2 (w !! 2),read $ init $ drop 2 (w !! 3)),(read $ init $ drop 2 (w !! 8),read $ drop 2 (w !! 9)))
            where w = words xs

putNoBeaconOnMap2000000 :: (Coord, Coord) -> Elemap -> Elemap
--putNoBeaconOnMap2000000 (sensor, beacon) elamp | trace ("sensor: " ++ show sensor ++ "\nbeacon: " ++ show beacon) False = undefined
putNoBeaconOnMap2000000 ((sensorx, sensory), (beaconx, beacony)) elmap =
    let dist = abs (sensorx - beaconx) + abs (sensory - beacony)
        reachable = [(x,2000000) | x <- [(sensorx-dist)..(sensorx + dist)], abs (sensorx - x) + abs (sensory - 2000000) <= dist]
    in foldl (\map coord -> insert coord NoBeacon map) elmap reachable

putSensorAndBeaconOnMap :: (Coord, Coord) -> Elemap -> Elemap
putSensorAndBeaconOnMap ((sensorx, sensory), (beaconx, beacony)) elmap =insert (beaconx, beacony) Beacon $ insert (sensorx, sensory) Sensor elmap

putEdgesOnMap :: (Coord, Coord) -> Edgemap -> Edgemap
--putEdgesOnMap (sensor, beacon) elamp | trace ("sensor: " ++ show sensor ++ "\nbeacon: " ++ show beacon ++ "\nsize: " ++ show (size elamp) ++ "\n") False = undefined
putEdgesOnMap ((sensorx, sensory), (beaconx, beacony)) elmap =
    let dist = abs (sensorx - beaconx) + abs (sensory - beacony)
        reachable = [(sensorx + d, sensory + y * (dist - abs d)) |
           d <- [(- dist) .. dist],
           sensorx + d > - 1,
           sensorx + d < 4000001,
           y <- [1, - 1],
           sensory + y * (dist - abs d) > - 1,
           sensory + y * (dist - abs d) < 4000001]
    in foldl (\map coord -> insert coord True map) elmap reachable

findIsolated :: [Coord] -> Edgemap -> [Coord]
findIsolated [] map = []
findIsolated (curr@(x,y):xs) edgeMap
            | x + 1 <= 4000000 && member (x + 2, y) edgeMap && member (x + 1, y + 1) edgeMap && member (x + 1, y - 1) edgeMap = (x + 1, y) : findIsolated xs edgeMap -- right
            | x - 1 >= 0       && member (x - 2, y) edgeMap && member (x - 1, y + 1) edgeMap && member (x - 1, y - 1) edgeMap = (x + 1, y) : findIsolated xs edgeMap -- left
            | y + 1 <= 4000000 && member (x, y + 2) edgeMap && member (x + 1, y + 1) edgeMap && member (x - 1, y + 1) edgeMap = (x, y + 1) : findIsolated xs edgeMap -- up
            | y - 1 >= 0       && member (x, y - 2) edgeMap && member (x + 1, y - 1) edgeMap && member (x - 1, y - 1) edgeMap = (x, y - 1) : findIsolated xs edgeMap -- up
            | otherwise = findIsolated xs edgeMap

checkCoord :: Coord -> [(Coord, Coord)] -> Bool
checkCoord coord [] = True
checkCoord (cx, cy) (curr@((sensorx, sensory),(beaconx, beacony)):xs)
    | abs (cx - sensorx) + abs (cy - sensory) <= dist = False
    | otherwise = checkCoord (cx, cy) xs
    where dist = abs (sensorx - beaconx) + abs (sensory - beacony)

checkAll :: [Coord] -> [(Coord, Coord)] -> Coord
checkAll [] xs = error "found none"
checkAll (x:xs) ys 
    | checkCoord x ys = x
    | otherwise = checkAll xs ys

countNoBeaconAtN :: Elemap -> Int
countNoBeaconAtN elmap = size $ Data.Map.filter (==NoBeacon) elmap    
solve xs = countNoBeaconAtN $ foldl (flip putSensorAndBeaconOnMap) (foldl (flip putNoBeaconOnMap2000000) empty (parse xs)) (parse xs)
solve2 xs = (fst res * 4000000) + snd res
    where res = checkAll (findIsolated (keys edges) edges) (parse xs)
            where edges = foldl (flip putEdgesOnMap) empty (parse xs) 
main = do
    input <- getContents
    print $ solve input      
    print $ solve2 input