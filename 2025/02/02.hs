-- very inefficient and ugly solution, but no time to optimize...


import qualified Data.Text as T

readNums :: [Char] -> [[Int]]
readNums xs =  map (map (read . T.unpack)) ls
  where ls =  map (T.splitOn (T.pack "-")) $ T.splitOn (T.pack ",") $ T.pack xs

-- Part 1:
isValid :: Int -> Bool
isValid num = if fst spl == snd spl then True else False
  where
    digits = show num
    spl = ((take (div (length digits) 2) digits), drop (div (length digits) 2) digits)

addUp :: Int -> [Int] -> Int
addUp val [] = val
addUp val (x:xs) = if isValid x then addUp (val + x) xs else addUp val xs

getRange :: [Int] -> [Int]
getRange [] = []
getRange (x:[]) = []
getRange (x:y:xs) = [x..y]

solve :: [Char] -> Int
solve xs = sum $ map (addUp 0 . getRange) (readNums xs)

-- Part 2:
splitEqual size [] = []
splitEqual size xs = (take size xs):splitEqual size (drop size xs)

allEqual [] = True
allEqual (x:[]) = True
allEqual (x:y:xs) = if x == y then allEqual (y:xs) else False


isValid2 :: Int -> Int -> Bool
isValid2 size xs
  | size > div (length digits) 2 = False
  | allEqual (splitEqual size digits) = True
  | otherwise =  isValid2 (size + 1) xs
  where digits = show xs


addUp2 :: Int -> [Int] -> Int
addUp2 val [] = val
addUp2 val (x:xs) = if isValid2 1 x then addUp2 (val + x) xs else addUp2 val xs


solve2 xs = sum $ map (addUp2 0 . getRange) (readNums xs)

main = do
  input <- getContents
  print $ "Part1: " ++ show (solve input)
  print $ "Part2: " ++ show (solve2 input)

