import Data.Text (pack, unpack, splitOn, breakOn, strip)
import Data.Char (isDigit)
import Debug.Trace (trace)
import Data.List (sortBy)
type Pair = ([Char], [Char])

parse :: [Char] -> [Pair]
parse xs = map (getChar . breakOn (pack "\n")) (splitOn (pack "\n\n") (pack xs))
    where getChar (left, right) = (unpack left, unpack $ strip right)


pairsToList :: [Pair] -> [[Char]]
pairsToList [] = []
pairsToList (x:xs) = fst x : snd x : pairsToList xs

injectDividers :: [[Char]] -> [[Char]]
injectDividers xs = "[[2]]" : "[[6]]" : xs

comp a b 
    | x = LT
    | not x = GT
    | otherwise = EQ
    where x = testPair (a,b)

testPair :: Pair -> Bool
--testPair (l, r) | trace ("testPair: l="++l++" r="++r) False = undefined
testPair (l:left, r:right)
    | l == '[' = case r of '[' -> testPair (left, right) -- both open new list
                           ']' -> False -- right side ran out first
                           x   -> case head right of ',' -> testPair (l:left, '[':r:']':right)
                                                     ']' -> testPair (l:left, '[':r:']':right)
                                                     x   -> testPair (l:left, '[':r:head right:']':tail right)
    | l == ']' = case r of ']' -> testPair (left, right)
                           ',' -> True
                           x   -> True --left side ran out first     
    | l == ',' = case r of ',' -> testPair (left, right)
                           x   -> testPair (left, r:right)
    | isDigit l && (head left == ']' || head left == ',') = case r of '[' -> testPair ('[':l:']':left, r:right)
                                                                      ',' -> testPair (l:left, right)
                                                                      ']' -> False -- right side ran out first
                                                                      x   -> case head right of ',' -> (l < r) || (l <= r && testPair (left, right))
                                                                                                ']' -> (l < r) || (l <= r && testPair (left, right))
                                                                                                xx  -> True
    | isDigit  l && isDigit (head left) = case r of '[' -> testPair (l:left, right)
                                                    ',' -> testPair (l:left, right)
                                                    ']' -> False -- right side ran out first
                                                    x   -> case head right of ',' -> False
                                                                              ']' -> False
                                                                              xx  -> testPair (tail left, tail right)
    | otherwise = error ("ERROR: pattern not matched: left=" ++ (l:left) ++ " right=" ++ (r:right))
testPair _ = error "ERROR: left or right must be empty"

getIndexes :: [Bool] -> Int -> [Int]
getIndexes [] _ = []
getIndexes (x:xs) n = (if x then n else 0) : getIndexes xs (n+1)

getDividerIndexes :: [[Char]] -> Int -> [Int]
getDividerIndexes [] _ = []
getDividerIndexes (x:xs) n = (if x=="[[2]]" || x == "[[6]]" then n else 1) : getDividerIndexes xs (n+1)
solve xs = sum $ getIndexes (map testPair (parse xs)) 1
solve2 xs = product $ getDividerIndexes (sortBy comp (injectDividers $ pairsToList $ parse xs)) 1

--solve3 xs = sortBy comp (injectDividers $ pairsToList $ parse xs)
main = do
    input <- getContents
    print $ solve input
    print $ solve2 input
    --print $ solve3 input


 