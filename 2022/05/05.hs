-- Crate will be a Char, Stack will be a List of Chars
-- Instruction will be a List of Ints:  [amount,from,to]

import qualified Data.Text as T
import qualified Data.Char as C

determineHeightOfStack :: [Char] -> Int
determineHeightOfStack (x:xs)
  | x == '\n' = 1 + determineHeightOfStack xs
  | x == 'm' = (-2)
  | otherwise = 0 + determineHeightOfStack xs

strip xs = [x | x <- xs, C.isAlphaNum $ head x]

parseStack :: [Char] -> [[Char]]
parseStack xs = strip [T.unpack $ T.strip x | x <- T.transpose $ map T.pack $ take (determineHeightOfStack xs) (lines xs), T.strip x /= T.pack ""]

parseInstr :: [Char] -> [[Int]]
parseInstr xs = map (map read . choose . words) $ drop ((determineHeightOfStack xs) + 2) (lines xs)
  where choose xs = [xs !! 1, xs !! 3, xs !! 5]

--part1
applyInstr :: [Int] -> [[Char]] -> [[Char]]
applyInstr [x,y,z] xs
  | x > 0 = applyInstr [x-1,y,z] final
  | otherwise = xs
  where modi = take (z-1) xs ++ ((head $ xs !! (y-1)) : (xs !! (z-1))) : drop z xs
        final = take (y-1) modi ++ (drop 1 (modi !! (y-1))) : drop y modi --magic
applyInstr _ _ = [[]]

applyAllInstr :: [[Int]] -> [[Char]] -> [[Char]]
applyAllInstr [] ys = ys
applyAllInstr (x:xs) ys = applyAllInstr xs (applyInstr x ys)

solve xs = map head (applyAllInstr (parseInstr xs) (parseStack xs))

--part2
applyInstr2 :: [Int] -> [[Char]] -> [[Char]]
applyInstr2 [x,y,z] xs = final
  where modi = take (z-1) xs ++ ((take x $ xs !! (y-1)) ++ (xs !! (z-1))) : drop z xs
        final = take (y-1) modi ++ (drop x (modi !! (y-1))) : drop y modi --dont ask
applyInstr2 _ _ = [[]]

applyAllInstr2 :: [[Int]] -> [[Char]] -> [[Char]]
applyAllInstr2 [] ys = ys
applyAllInstr2 (x:xs) ys = applyAllInstr2 xs (applyInstr2 x ys)

solve2 xs = map head (applyAllInstr2 (parseInstr xs) (parseStack xs))


main = do
  input <- getContents
  print $ solve input
  print $ solve2 input
