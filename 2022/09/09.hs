import Data.Set (size, fromList)

type Coord = (Int, Int)
type Rope = ([Coord], [Coord])

-- part 1
shortRope :: Rope
shortRope = ([(0,0), (0,0)], [(0,0)])

-- part 2
longRope :: Rope
longRope = ([(0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0)], [(0,0)])

-- aligns a pair of knots according to the movement rules - returns only the moved tail (the head already moved before)
alignPair :: (Coord, Coord) -> Coord
alignPair (head@(hx, hy), tail@(tx, ty))
    | abs (hx-tx) <= 1 && abs (hy-ty) <= 1 = tail
    | hx == tx + 2 = case () of _
                                    | hy == ty + 1 || hy == ty + 2 -> (tx + 1, ty + 1)
                                    | hy == ty                     -> (tx + 1, ty)
                                    | hy == ty - 1 || hy == ty - 2 -> (tx + 1, ty - 1)
                                    | otherwise -> error "ERROR: alignPair -> hx == tx + 2 - pattern should be unreachable"
    | hx == tx - 2 = case () of _
                                    | hy == ty + 1 || hy == ty + 2 -> (tx - 1, ty + 1)
                                    | hy == ty                     -> (tx - 1, ty)
                                    | hy == ty - 1 || hy == ty - 2 -> (tx - 1, ty - 1)
                                    | otherwise -> error "ERROR: alignPair -> hx == tx - 2 - pattern should be unreachable"
    | hy == ty + 2 = case () of _ 
                                    | hx == tx + 1 -> (tx + 1, ty + 1)
                                    | hx == tx     -> (tx, ty + 1)
                                    | hx == tx - 1 -> (tx - 1, ty + 1)
                                    | otherwise -> error "ERROR: alignPair -> hy == ty + 2 - pattern should be unreachable"
    | hy == ty - 2 = case () of _ 
                                    | hx == tx + 1 -> (tx + 1, ty - 1)
                                    | hx == tx     -> (tx, ty - 1)
                                    | hx == tx - 1 -> (tx - 1, ty - 1)
                                    | otherwise -> error "ERROR: alignPair -> hy == ty - 2 - pattern should be unreachable"
    | otherwise = error ("ERROR: alignPair pattern should be unreachable, hx=" ++ show hx ++ ", hy=" ++ show hy ++ ", tx=" ++show tx ++ ", ty=" ++ show ty)

-- sends pairs to alignPair to be aligned according to the movement rules
alignAll :: Rope -> Rope
alignAll (x:xs, ls) = (alignAll x xs, last (alignAll x xs) : ls)
    where alignAll :: Coord -> [Coord] -> [Coord]
          alignAll x [] = [x]
          alignAll x (y:ys) = x : alignAll (alignPair (x,y))  ys
alignAll ([], _) = ([],[])

parseMove :: [Char] -> (Char, Int)
parseMove xs = (head xs, read $ drop 2 xs)

-- moves the head according to the input and then sends the rest to be aligned
applyMove :: (Char, Int) -> Rope -> Rope
applyMove (_, 0) rope = rope
applyMove (char, n) ((x,y):rs, ls)
    | char == 'U' = applyMove (char, n-1) $ alignAll ((x, y + 1):rs, ls)
    | char == 'D' = applyMove (char, n-1) $ alignAll ((x, y - 1):rs, ls)
    | char == 'L' = applyMove (char, n-1) $ alignAll ((x - 1, y):rs, ls)
    | char == 'R' = applyMove (char, n-1) $ alignAll ((x + 1, y):rs, ls)
    | otherwise = error "ERROR: invalid input"
applyMove _ rope = error ("ERROR: applyMove - pattern should be unreachable, rope: " ++ show rope)

applyAllMoves :: [[Char]] -> Rope -> Rope
applyAllMoves xs rope
  = foldl (\ rope x -> applyMove (parseMove x) rope) rope xs

ropeSize :: Rope -> Int
ropeSize (_, xs) = size $ fromList xs

solve :: String -> Int
solve xs = ropeSize $ applyAllMoves (lines xs) shortRope
solve2 :: String -> Int
solve2 xs = ropeSize $ applyAllMoves (lines xs) longRope

main = do
    input <- getContents
    print $ solve input
    print $ solve2 input