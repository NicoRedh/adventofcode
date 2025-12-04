-- I know this is very inefficient but it finished soon enough on my laptop and there was no time to optimise


type Grid = [[(Int, Int)]] -- The first Int is 1 for paper roll, 0 for no paper roll. The second Int is count of neighbors.

-- takes an index and a two dimensional list and increments the index
setIndex :: Int -> (Int, Int) -> Grid -> Grid
setIndex val (x,y) grid = init (fst splX) ++ [init (fst splY) ++ [((fst (last (fst splY))), (snd (last (fst splY))) + val)] ++ snd splY]  ++ snd splX
  where splX = splitAt (x + 1) grid
        splY = splitAt (y + 1) (last (fst splX))

calc :: (Int, Int) -> Grid -> Grid
calc (x, y) grid
  | x >= xlen = grid                          -- finished the last row
  | y >= ylen = calc (x + 1, 0) grid          -- finished one row -> move to the next
  | fst (grid !! x !! y) == 0 = calc (x, y + 1) grid
  | otherwise = calc (x, y + 1) (setIndex value (x,y) grid)
  where ylen = length (grid !! 0)
        xlen = length grid
        value = ((if x /= 0                          && fst (grid !! (x - 1) !! y) == 1       then 1 else 0) 
                + (if x /= xlen - 1                  && fst (grid !! (x + 1) !! y) == 1       then 1 else 0)
                + (if y /= 0                         && fst (grid !! x !! (y - 1)) == 1       then 1 else 0)
                + (if y /= ylen - 1                  && fst (grid !! x !! (y + 1)) == 1       then 1 else 0)
                + (if x /= 0        && y /= 0        && fst (grid !! (x - 1) !! (y - 1)) == 1 then 1 else 0)
                + (if x /= xlen - 1 && y /= 0        && fst (grid !! (x + 1) !! (y - 1)) == 1 then 1 else 0)
                + (if x /= 0        && y /= ylen - 1 && fst (grid !! (x - 1) !! (y + 1)) == 1 then 1 else 0)
                + (if x /= xlen - 1 && y /= ylen - 1 && fst (grid !! (x + 1) !! (y + 1)) == 1 then 1 else 0))                                                                        

parse :: [Char] -> [(Int, Int)]
parse [] = []
parse (x:xs)
  | x == '.' = (0, 0): parse xs
  | x == '@' = (1, 0): parse xs
  | otherwise = error ("ERROR: could not parse input!")


countWhenAbove :: Int -> [(Int, Int)] -> Int
countWhenAbove num [] = 0
countWhenAbove num ((r, x):xs) = if x < num && r == 1 then 1 + countWhenAbove num xs else countWhenAbove num xs

solve xs =sum $ map (countWhenAbove 4) (calc (0,0) (map parse (words xs)))

-- Part 2:
resetLine :: Int -> [(Int, Int)] -> [(Int, Int)]
resetLine num [] = []
resetLine num ((r, x):xs) = if x < num && r == 1 then (0,0) : resetLine num xs else if r == 1 then (1, 0) : resetLine num xs else (0,0) : resetLine num xs

iter initGrid
  | value == 0 = 0
  | otherwise = value + iter (map (resetLine 4) transformedGrid)
    where transformedGrid = calc (0,0) initGrid
          value = sum $ map (countWhenAbove 4) transformedGrid

solve2 xs = iter (map parse (words xs))          


x = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@."
y = map parse (words x)
z = calc (0,0) y

main = do
 input <- getContents
 print $ "Part 1: " ++ show (solve input)
 print $ "Part 2: " ++ show (solve2 input)
