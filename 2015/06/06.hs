splitInThree p1 p2 xs = [take p1 xs, take (p2-p1) $ drop p1 xs, drop p2 xs]

mapSnd f (x:y:z) = x:(map f y):z

on x1 y1 x2 y2 light = concat $ mapSnd (\a -> concat $ mapSnd turn (splitInThree (y1-1) y2 a)) (splitInThree (x1-1) x2 light)
  where turn _ = 1

on2 x1 y1 x2 y2 light = concat $ mapSnd (\a -> concat $ mapSnd turn (splitInThree (y1-1) y2 a)) (splitInThree (x1-1) x2 light)
  where turn x = x + 1
        
        

off x1 y1 x2 y2 light = concat $ mapSnd (\a -> concat $ mapSnd turn (splitInThree (y1-1) y2 a)) (splitInThree (x1-1) x2 light)
  where turn _ = 0

off2 x1 y1 x2 y2 light = concat $ mapSnd (\a -> concat $ mapSnd turn (splitInThree (y1-1) y2 a)) (splitInThree (x1-1) x2 light)
  where turn 0 = 0
        turn x = x - 1

toggle x1 y1 x2 y2 light = concat $ mapSnd (\a -> concat $ mapSnd turn (splitInThree (y1-1) y2 a)) (splitInThree (x1-1) x2 light)
  where turn 0 = 1
        turn 1 = 0

toggle2 x1 y1 x2 y2 light = concat $ mapSnd (\a -> concat $ mapSnd turn (splitInThree (y1-1) y2 a)) (splitInThree (x1-1) x2 light)
  where turn x = x + 2


-- returns List of Strings in Form [function, x1, y1, x2, y2]
readStr :: [Char] -> [[Char]]
readStr xs
  | take 2 xs == "tu" = (take 3 $ drop 1 ws) ++ drop 5 ws
  | otherwise = take 3 ws ++ drop 4 ws
  where ws = words [(\a -> if a == ',' then ' ' else a) x | x <- xs]

solve1 [] ls = ls
solve1 (x:xs) ls = let lss = if head x == "off" then off x1 y1 x2 y2 ls
                             else if head x == "on" then on x1 y1 x2 y2 ls
                             else toggle x1 y1 x2 y2 ls
                   in solve1 xs lss
                   where x1 = read (head $ tail x) :: Int
                         y1 = read (head $ drop 2 x) :: Int
                         x2 = read (head $ drop 3 x) :: Int
                         y2 = read (last x) :: Int

solve2 [] ls = ls
solve2 (x:xs) ls = let lss = if head x == "off" then off2 x1 y1 x2 y2 ls
                             else if head x == "on" then on2 x1 y1 x2 y2 ls
                             else toggle2 x1 y1 x2 y2 ls
                   in solve2 xs lss
                   where x1 = read (head $ tail x) :: Int
                         y1 = read (head $ drop 2 x) :: Int
                         x2 = read (head $ drop 3 x) :: Int
                         y2 = read (last x) :: Int

                         
count xs = sum $ map sum xs


main = do
  input <- getContents
  print ("Part1: " ++ (show $ count $ solve1 (map readStr $ lines input) (replicate 1000 $ replicate 1000 0)))
  print ("Part2: " ++ (show $ count $ solve2 (map readStr $ lines input) (replicate 1000 $ replicate 1000 0)))
  


