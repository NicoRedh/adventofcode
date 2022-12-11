import Data.Text (pack, unpack, splitOn)
import Debug.Trace (trace)
import Data.List (sort)
import System.Posix.Internals (lstat)
-- ([((items, op, test, (true, false), inspections):mk)], (monkeyNo, itemNo))

--             Items  Operation   Test         True, False Inspections
type Monkey = ([Int], Int -> Int, Int -> Bool, (Int, Int), Int)
--          (MonkeyNo, ItemNo)
type Turn = (Int, Int)
type GameState = ([Monkey], Turn)

parse :: [Char] -> [Monkey]
parse xs = map (parseMonkey . unpack) $ splitOn (pack "\n\n") (pack xs)
    where parseMonkey :: [Char] -> Monkey
          parseMonkey xs
            | null xs = error "ERROR: parse - the last element was probably still empty - take init maybe"
            | otherwise = (items, op, test, (true, false), 0)
            where text = tail $ lines xs
                  items = map (read . unpack) $ splitOn (pack ", ") (pack $ drop 18 (head text))
                  operator = head $ words $ drop 23 $ text !! 1
                  operee = last $ words $ drop 23 $ text !! 1
                  op = case operator of "*" -> case operee of  "old" -> (\a -> a * a)
                                                               x     -> (\a -> a * read x)
                                        "+" -> case operee of  "old" -> (\a -> a + a)
                                                               x     -> (\a -> a + read x)
                                        _ -> error "ERROR: op is not * or +"
                  test = \a -> mod a (read $ drop 21 (text !! 2)) == 0
                  true = read $ last $ words (text !! 3)
                  false = read $ last $ words (text !! 4)


getItems :: Monkey -> [Int]
getItems (items, _, _, _, _) = items

getInspection :: Monkey -> Int
getInspection (_, _, _, _, i) = i

findLeastCommonMultiple :: [Monkey] -> Int
findLeastCommonMultiple xs = product $ map findMod xs
    where findMod (_, _, test, _, _) = head [x | x <- [1..], test x]

calcWorryLevel :: Monkey -> Int
calcWorryLevel (items, op, _, _, _) = div (op (head items)) 3

calcWorryLevel2 :: Monkey -> Int
calcWorryLevel2 (items, op, _, _, _) = op (head items)

detNextMonkey :: Monkey -> Int -> Int
detNextMonkey (_, _, test, (true, false), _) worryLevel = if test worryLevel then true else false

takeTurn :: GameState -> GameState
--takeTurn (monkeys, (monkeyNo, itemNo)) | trace ("monkey items: " ++ show (map getItems monkeys) ++ " monkeyNo: " ++ show monkeyNo ++ " itemNo: " ++ show itemNo) False = undefined
takeTurn (monkeys, (monkeyNo, itemNo))
    | monkeyNo >= length monkeys = (monkeys, (0, 0))
    | null (getItems $ monkeys !! monkeyNo) = takeTurn (monkeys, (monkeyNo + 1, 0))
    | otherwise = takeTurn (newMonkeys, (monkeyNo, itemNo + 1))
    where sendMonkey = monkeys !! monkeyNo
          worryLevel = calcWorryLevel sendMonkey
          nextMonkeyNo = detNextMonkey sendMonkey worryLevel
          nextMonkey = monkeys !! nextMonkeyNo
          newSender (items, op, test, (true, false), inspections) = (tail items, op, test, (true, false), inspections + 1)
          newReceiver (items, op, test, (true, false), inspections) = (items ++ [worryLevel], op, test, (true,false), inspections)
          a = min monkeyNo nextMonkeyNo
          b = max monkeyNo nextMonkeyNo
          lower = if a == monkeyNo then newSender sendMonkey else newReceiver nextMonkey
          greater = if b == monkeyNo then newSender sendMonkey else newReceiver nextMonkey
          newMonkeys = take a monkeys ++ [lower] ++ take (b - a - 1) (drop (a + 1) monkeys) ++ [greater] ++ drop (b+1) monkeys


takeTurn2 :: GameState -> GameState
--takeTurn2 (monkeys, (monkeyNo, itemNo)) | trace ("monkey items: " ++ show (map getItems monkeys) ++ " monkeyNo: " ++ show monkeyNo ++ " itemNo: " ++ show itemNo) False = undefined
takeTurn2 (monkeys, (monkeyNo, itemNo))
    | monkeyNo >= length monkeys = (monkeys, (0, 0))
    | null (getItems $ monkeys !! monkeyNo) = takeTurn2 (monkeys, (monkeyNo + 1, 0))
    | otherwise = takeTurn2 (newMonkeys, (monkeyNo, itemNo + 1))
    where sendMonkey = monkeys !! monkeyNo
          worryLevel = mod (calcWorryLevel2 sendMonkey) (findLeastCommonMultiple monkeys)
          nextMonkeyNo = detNextMonkey sendMonkey worryLevel
          nextMonkey = monkeys !! nextMonkeyNo
          newSender (items, op, test, (true, false), inspections) = (tail items, op, test, (true, false), inspections + 1)
          newReceiver (items, op, test, (true, false), inspections) = (items ++ [worryLevel], op, test, (true,false), inspections)
          a = min monkeyNo nextMonkeyNo
          b = max monkeyNo nextMonkeyNo
          lower = if a == monkeyNo then newSender sendMonkey else newReceiver nextMonkey
          greater = if b == monkeyNo then newSender sendMonkey else newReceiver nextMonkey
          newMonkeys = take a monkeys ++ [lower] ++ take (b - a - 1) (drop (a + 1) monkeys) ++ [greater] ++ drop (b+1) monkeys


--takeNTurns :: Int -> GameState -> GameState
--takeNTurns n (monkeys, turn) | trace ("n: " ++ show n ++ "  monkey items: " ++ show (map getItems monkeys)) False = undefined
takeNTurns :: Int -> GameState -> (GameState -> GameState) -> GameState
takeNTurns 0 gs f = gs
takeNTurns n gs f = takeNTurns (n-1) (f gs) f

solve xs = last res * res !! (length res - 2)
    where res =  sort $ map getInspection $ fst $ takeNTurns 20  (parse xs, (0,0)) takeTurn

solve2 xs = last res * res !! (length res - 2)
    where res =  sort $ map getInspection $ fst $ takeNTurns 10000 (parse xs, (0,0)) takeTurn2

main = do
    input <- getContents
    print $ solve input
    print $ solve2 input