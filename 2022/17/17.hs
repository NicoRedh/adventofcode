import Data.Set (Set, member, insert, toList, empty, null, size, filter, map, (\\))
import Data.List (maximumBy)
import Data.Function (on)
import Debug.Trace (trace)
type Coord = (Int, Int)
type Field = Set Coord
type Block = [Coord]
data Shape = Bar | Cross | Cane | Stick | Square deriving (Show, Eq)
data Move = L | R deriving (Show)


generateShape :: Coord -> Shape -> [Coord]
generateShape (x,y) shape = case shape of Bar    -> [(h, y + 4) | h <- [2..5]]
                                          Cross  -> [(3,y + 6), (3, y + 4), (2, y + 5), (3, y + 5), (4, y + 5)]
                                          Cane   -> [(2, y + 4), (3, y + 4), (4, y + 4), (4, y + 5), (4, y + 6)]
                                          Stick  -> [(2, y + h) | h <- [4..7]]
                                          Square -> [(2, y + 4), (2, y + 5), (3, y + 4), (3, y + 5)]

parse :: [Char] -> [Move]
parse [] = []
parse (x:xs)
  | x == '<' = L : parse xs
  | otherwise = R : parse xs

-- The floor of the playing field is y == 0.
-- The width of the playing field is 0 <= x <= 6
-- The starting position is x == 3 (considering each block as a 3x3 unit)

applyMove :: Move -> Block -> Field -> Block
applyMove L coords field
  | moveable = Prelude.map (\(x,y) -> (x-1, y)) coords
  | otherwise = coords
  where moveable = not (foldl (||) False (Prelude.map (\(x,y) -> member (x-1,y) field || x - 1 < 0) coords))
applyMove R coords field
  | moveable = Prelude.map (\(x,y) -> (x+1, y)) coords
  | otherwise = coords
  where moveable = if length coords == 0 then error ("ERROR: applyMove R") else  not (foldl (||) False (Prelude.map (\(x,y) -> member (x+1,y) field || x + 1 > 6) coords))

moveDown :: Block -> Field -> Block
moveDown coords field = Prelude.map (\(x,y) -> (x, y - 1)) coords

settleDown :: Block -> Field -> Field
settleDown coords field = foldl (\set coord -> insert coord set) field coords

canMoveDown coords field = if length coords == 0 then error ("ERROR: canMoveDown") else not (foldl (||) False (Prelude.map (\(a,b) -> member (a, b - 1) field || b - 1 <= 0) coords))

tetris :: ([Shape],Int) -> [Move] -> (Field, Int) -> Int
--tetris ((x:xs),counter) moves (field,index) | trace (("Size of Field: " ++ show (size field)) ++ "\t Progress: " ++ show counter) False = undefined
tetris ([],counter) moves (field, index) = snd (if Data.Set.null field then (0,0) else maximumBy (compare `on` snd) (toList field))
tetris ((x:xs),counter) moves (field,index) = tetris (xs, counter + 1) moves (playBlock (generateShape highest x) moves (field, index))
  where highest = if Data.Set.null field then (0,0) else maximumBy (compare `on` snd) (toList field)
        playBlock :: [Coord] -> [Move] -> (Field, Int) -> (Field, Int)
        --playBlock coords moves (field, index) | trace ("PLAYBLOCK: Block = " ++ show coords) False = error ("ERROR: playBlock")
        playBlock coords moves (field, index) = if canMoveDown movedBlock field then playBlock (moveDown movedBlock field) moves (field, index + 1) else (settleDown movedBlock field, index + 1)
          where movedBlock = applyMove (moves !! (mod index (length moves))) coords field

heightOfField :: Field -> Int
heightOfField field = snd (if Data.Set.null field then (0,0) else maximumBy (compare `on` snd) (toList field))

solve xs= tetris ((take 2022 (cycle [Bar, Cross, Cane, Stick, Square])),0) (parse xs) (empty, 0)
-- sadly no programmatic solution, only through debugging and recognizing pattern.
-- works only for my input.
solve2 xs = 581395347 * 2626 + tetris ((take (1720+1440) (cycle [Bar, Cross, Cane, Stick, Square])),0) (parse xs) (empty, 0)

main = do
 input <- getLine
 print $ solve input
 print $ solve2 input
