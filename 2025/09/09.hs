import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import Data.List (maximumBy)
import Debug.Trace (trace)


parse :: [Char] -> [(Int, Int)]
parse xs = map p (lines xs)
  where p l = (getTuple . map (read . T.unpack) . T.splitOn (T.pack ",")) (T.pack l)
        getTuple (x:y:_) = (x,y)
        
manhattan (x,y) (a,b) = abs (x - a) + abs (y - b)

area (x,y) (a,b) = (abs (x-a) + 1) * (abs (y - b) + 1)

solve xs = area (fst m) (snd m)
  where m = snd $  maximum [(manhattan a b, (a,b)) | a <- input, b <- input, a<b]
        input = parse xs

-- Part 2:
-- The following solution took me 75 minutes and 25 seconds on my machine.
-- This is obviously not what was asked here, but I am happy to have found a functioning solution.

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

getBorderSet :: [(Int,Int)] -> S.Set (Int,Int)
getBorderSet ((a,b):(c,d):_) = S.fromList $
                               [(l, x) | x <- [m..o]]
                               ++ [(x, o) | x <- [l..n]]
                               ++ [(x, m) | x <- [l..n]]
                               ++ [(n, x) | x <- [m..o]]
  where l = min a c
        m = min b d
        n = max a c
        o = max b d

getBorderList :: [(Int,Int)] -> [(Int,Int)]
getBorderList ((a,b):(c,d):_) = [(l, x) | x <- [m..o]]
                               ++ [(x, o) | x <- [l..n]]
                               ++ [(x, m) | x <- [l..n]]
                               ++ [(n, x) | x <- [m..o]]
  where l = min a c
        m = min b d
        n = max a c
        o = max b d

getMap :: S.Set (Int,Int) -> M.Map Int (S.Set Int)
getMap border = M.map S.fromList tmp
  where tmp = S.foldr (\ele m -> M.insertWith (++) (snd ele) [fst ele] m) M.empty border

checkRect :: M.Map Int (S.Set Int) -> [(Int, Int)] -> Bool
--checkRect border rectBorder | trace ("CheckRect\n Border:\n\n" ++ show border ++ "\nrectBorder:\n\n" ++ show rectBorder ++ "\n") False = undefined
checkRect border (r:rectBorder) 
  | null rectBorder = checkGreen r
  | not (checkGreen r) = False
  | otherwise = checkRect border rectBorder
  where
    pts :: (Int, Int) -> S.Set Int
    pts point = fromJust (M.lookup (snd point) border)
    checkGreen point = S.member (fst point) (pts point) || countCollisions 0 (S.takeWhileAntitone (<= (fst point)) (pts point))
    --countCollisions n xs | trace ("countCollisions: \t n: " ++ show n ++ "\t xs: " ++ show xs ++ "\n") False = undefined
    countCollisions n xs = odd $ S.size (S.filter (\f -> not (S.member (f-1) xs)) xs)


findLargest :: [((Int, Int), (Int, Int))] -> M.Map Int (S.Set Int) -> Int
findLargest distances border | trace ("Remaining rectangles: " ++ show (length distances)) False = undefined
findLargest (d:distances) border
  | checkRect border (getBorderList [fst d, snd d]) = area (fst d) (snd d)
  | otherwise = findLargest distances border

createBorder :: [(Int, Int)] -> S.Set (Int, Int)
createBorder xs = S.unions $ map (getBorderSet . detuplify) pairs
  where pairs = (last xs, head xs) : (zip xs $ tail xs)
        detuplify (a,b) = [a,b]

solve2 xs = findLargest distances (getMap border)
    where input = parse xs
          distances = (map snd . reverse . L.sortOn fst) [(manhattan a b, (a,b)) | a <- input, b <- input, a<b]
          border = createBorder input

main = do
  input <- getContents
  print $ "Part 1: " ++ show (solve input)
  print $ "Part 2: " ++ show (solve2 input)
  


