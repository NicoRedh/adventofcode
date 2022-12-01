import Data.List (sort)
enlist ch xs = words $ [repl x | x <- xs] where repl a = if a == ch then ' ' else a


calc [a,b,c] = 3*a*b + 2*a*c + 2*b*c
calc _ = 0

calcRib [a,b,c] = 2*a + 2*b + a*b*c
calcRib _ = 0

solve xs =sum $ map ( calc . sort . map read . enlist 'x') (enlist '\n' xs)

solve2 xs =sum $ map ( calcRib . sort . map read . enlist 'x') (enlist '\n' xs)


main :: IO ()
main = do
    input <- getContents
    print ("Part1: " ++ show (solve input))
    print ("Part2: " ++ show (solve2 input))
    