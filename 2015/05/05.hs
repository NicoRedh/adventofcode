vowels :: [Char] -> Bool
vowels xs = length ys > 2 where ys = [x | x <- xs, x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u']

doubleletter :: [Char] -> Bool
doubleletter [] = False
doubleletter [a] = False
doubleletter (x:y:ys) = if x == y then True else doubleletter (y:ys)

substr :: [Char] -> Bool
substr [] = True
substr [a] = True
substr (x:y:ys) = if elem (x, y) [('a','b'),('c','d'),('p','q'),('x','y')] then False else substr (y:ys)


repLet :: [Char] -> Bool
repLet [] = False
repLet [a] = False
repLet [a,b] = False
repLet (x:y:z:xs) = if x == z then True else repLet (y:z:xs)

twoLet :: [Char] -> Bool
twoLet [] = False
twoLet (x:y:xs) = if elem (x, y) (tupl xs) then True else twoLet (y:xs)
twoLet xs = False

tupl [] = []
tupl [a] = []
tupl [a, b] = [(a,b)]
tupl (x:y:ys) = [(x, y)] ++ tupl (y:ys)

check1 xs = if substr xs && vowels xs && doubleletter xs then 1 else 0
check2 xs = if twoLet xs && repLet xs then 1 else 0

solve1 xs = sum $ map check1 (words xs)
solve2 xs = sum $ map check2 (words xs)
main = do
  input <- getContents
  putStrLn ("Part1: " ++ (show $ solve1 input))
  putStrLn ("Part2: " ++ (show $ solve2 input))
