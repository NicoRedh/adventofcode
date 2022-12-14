import Data.Map (Map, insert, empty, keys, notMember, member)
import Data.Text (pack, unpack, splitOn, breakOn)
import Data.List (maximumBy)
import Data.Function (on)
import Debug.Trace (trace)

data Elem = Rock | Sand deriving (Show)
type Coord = (Int, Int)
type Elemap = Map Coord Elem

parse :: [Char] -> Elemap
parse xs = foldl parseLine empty (map (map ((\(a,b) -> (read (unpack a), read (tail (unpack b)))) . breakOn (pack ",")) . splitOn (pack " -> ") . pack) (lines xs))
    where parseLine :: Elemap -> [Coord] -> Elemap
          parseLine elemap [] = elemap
          parseLine elemap [x] = elemap
          parseLine elemap (x:y:xs) = parseLine (checkPair elemap (x,y)) (y:xs)
            where checkPair elmap ((a,b),(x,y))
                    | a < x && b == y = foldl (\map z -> insert (z,y) Rock map) elmap [a..x]
                    | a > x && b == y = foldl (\map z -> insert (z,y) Rock map) elmap [x..a]
                    | b < y && a == x = foldl (\map z -> insert (x,z) Rock map) elmap [b..y]
                    | b > y && a == x = foldl (\map z -> insert (x,z) Rock map) elmap [y..b]
                    |    otherwise = error "ERROR: could not parse input"

dropSand :: Int -> Coord -> Elemap -> Elemap
dropSand maxY start elmap = insert (detEndPos start) Sand elmap
    where detEndPos :: Coord -> Coord
          detEndPos (x,y) 
            | y > maxY = (-1, -1)
            | notMember (x, y + 1) elmap = detEndPos (x, y +1 )
            | notMember (x - 1, y + 1) elmap = detEndPos (x - 1, y + 1)
            | notMember (x + 1, y + 1) elmap = detEndPos (x + 1, y + 1)
            | otherwise = (x,y)  

dropSand2 :: Int -> Coord -> Elemap -> Elemap
dropSand2 maxY start elmap = insert (detEndPos start) Sand elmap
    where detEndPos :: Coord -> Coord
          detEndPos (x,y) 
            | y >= maxY + 1 = (x, y)
            | notMember (x, y + 1) elmap = detEndPos (x, y +1 )
            | notMember (x - 1, y + 1) elmap = detEndPos (x - 1, y + 1)
            | notMember (x + 1, y + 1) elmap = detEndPos (x + 1, y + 1)
            | otherwise = (x,y)  

dropAllSand :: Int -> Int -> Coord -> Elemap -> Int
dropAllSand maxY n start elmap
    | member (-1, -1) elmap = n - 1
    | otherwise = dropAllSand maxY (n + 1) start (dropSand maxY start elmap)

dropAllSand2 :: Int -> Int -> Coord -> Elemap -> Int
dropAllSand2 maxY n start elmap
    | member (500, 0) elmap = n
    | otherwise = dropAllSand2 maxY (n + 1) start (dropSand2 maxY start elmap)

solve xs = dropAllSand (snd $ maximumBy (compare `on` snd) (keys elmap)) 0 (500,0) elmap
    where elmap = parse xs
solve2 xs = dropAllSand2 (snd $ maximumBy (compare `on` snd) (keys elmap)) 0 (500,0) elmap
    where elmap = parse xs
main = do
    input <- getContents
    print $ solve input
    print $ solve2 input