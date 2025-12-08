import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.List as L
type Vec = (Float, Float, Float)
type Dist = (Float, (Vec, Vec))
type Circuit = S.Set Vec
---type Circuits = 
{-
1. calculate all distances
2. Connect smallest using sets
3. If one of smallest already in circuit, take union of two circuits
-}


parse :: [Char] -> [Vec]
parse xs = map r (lines xs)
  where r l = tuplify $ map (read . T.unpack) $ T.splitOn (T.pack ",") (T.pack l)
        tuplify (x:y:z:a) = (x,y,z)

eucDis :: Vec -> Vec -> Float
eucDis (a,b,c) (x,y,z) = sqrt $ sum [(a - x) ** 2, (b - y) ** 2, (c - z) ** 2]

getAllDists :: [Vec] -> [Dist]
getAllDists xs = [( eucDis a b, (a, b)) | a <- xs, b <- xs, a < b]

connectNLines :: Int -> [Circuit] -> S.Set Dist -> [Circuit] 
connectNLines n circuits dists
  | n == 0 = circuits
  | otherwise = connectNLines (n - 1) (fst newL) (snd newL)
  where newL = connectLine circuits dists

connectLine :: [Circuit] -> S.Set Dist -> ([Circuit], S.Set Dist)
connectLine circuits dists = (newC, snd m)
  where m = S.deleteFindMin dists
        minV = snd (fst m)
        newC = insertCirc [fst minV, snd minV] circuits

insertCirc :: [Vec] -> [Circuit] -> [Circuit]
insertCirc (x:y:_) cs = (S.unions ctn):nctn
  where css = (S.fromList [x, y]):cs
        ctn = [a | a <- css, S.member x a || S.member y a]
        nctn = [a | a <- css, not (S.member x a || S.member y a)]

getSizes :: [Circuit] -> [Int]
getSizes = map S.size

solve xs = foldr (*) 1 (drop (length sizes - 3) $ L.sort sizes)
  where sizes = getSizes $ connectNLines 1000 [] (S.fromList (getAllDists (parse xs)))

-- Part 2:

connectAllLines :: Int -> [Circuit] -> S.Set Dist -> Dist
connectAllLines  n circuits dists
  | check n (connectLine circuits dists) = head $ S.elems dists
  | otherwise = connectAllLines n (fst newL) (snd newL)
  where newL = connectLine circuits dists
  
check :: Int -> ([Circuit], S.Set Dist) -> Bool
check n (s, _)
  | length s == 1 && S.size (head s) == n = True
  | otherwise = False

solve2 xs = mulFin (fst lastCoords) (snd lastCoords)
  where lastElem = connectAllLines (length (lines xs)) [] (S.fromList (getAllDists (parse xs)))
        lastCoords = snd lastElem
        mulFin (a,_,_) (b,_,_) = (round a * round b)
main = do
  input <- getContents
  print ""
  print $ "Part 1: " ++ show (solve input)
  print $ "Part 2: " ++ show (solve2 input)


