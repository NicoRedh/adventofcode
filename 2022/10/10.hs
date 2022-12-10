data Instruction = Noop | Addx Int

type Register = [Int] -- (CycleNo, Value after CycleNo)
type Screen = [Char]

parse :: [Char] -> [Instruction]
parse xs = map (parseWords . words) $ lines xs
  where parseWords (x:xs)
          | x == "noop" = Noop
          | otherwise = Addx (read $ head xs)

instruct :: Register -> Instruction -> Register -- new values come in at the left of the list
instruct values Noop = head values : values
instruct values (Addx num) = (head values) + num : head values : values

instructAll :: [Instruction] -> Register -> Register -- result must be reversed for index to match cycleNo
instructAll [] reg = reg
instructAll (x:xs) reg = instructAll xs (instruct reg x)

valAtNthCycle :: Int -> Register -> Int
valAtNthCycle n reg = n * (head $ drop (n-1) reg)

drawScreen :: Int -> Screen -> Register -> Screen -- the register is right way round the screen is reversed
drawScreen 241 screen reg = screen 
drawScreen n screen reg
  | elem (mod n 40) [regAt n-1, regAt n, regAt n+1] = drawScreen (n+1) ('#' : screen) reg
  | otherwise = drawScreen (n+1) ('.' : screen) reg
  where regAt n
          | n-1 < 0 = reg !! 0
          | otherwise = reg !! n

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =if length xs >= n then take n xs : chunksOf n (drop n xs) else [] 


solve xs = valAtNthCycle 20 reg + valAtNthCycle 60 reg + valAtNthCycle 100 reg + valAtNthCycle 140 reg + valAtNthCycle 180 reg + valAtNthCycle 220 reg
  where reg = reverse $ instructAll (parse xs) [1]


solve2 xs = unlines $ chunksOf 40 $ reverse $ drawScreen 0 [] reg
  where reg = reverse $ instructAll (parse xs) [1]



main = do
  input <- getContents
  print $ solve input
  putStr $ solve2 input
