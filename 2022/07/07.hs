import Data.Char (isDigit)

type Name = String
type Data = (Int, String)
data Dir = File Data | Folder Name [Dir] deriving (Show, Eq)

--Crumbs idea inspired from binary tree in learn you a haskell for great good
type Crumbs = [Dir]

goDown :: [Char] -> (Dir, Crumbs) -> (Dir, Crumbs)
goDown dirName (Folder name dir, cs)
    | findDir dir `elem` dir = (findDir dir, Folder name withoutCurr:cs)
    | otherwise = error ("ERROR: could not go down: " ++ dirName)
    where findDir [] = error "ERROR: goDown - unreachable"
          findDir (Folder nm dr:xs) = if nm == dirName then Folder nm dr else findDir xs
          findDir (File fl:xs) = findDir xs
          withoutCurr = [x | x <- dir, x /= findDir dir]
goDown _ _ = error "ERROR: goDown - Pattern should not exist"


goUp :: (Dir, Crumbs) -> (Dir, Crumbs)
goUp (dir, (Folder nm dr):cs) = (Folder nm (dir:dr), cs)
goUp _ = error "ERROR - goUp - Pattern should not exist"

insertChild :: Dir -> Dir -> Dir
insertChild child (Folder name content) = Folder name (child:content)
insertChild _ _ = error "ERROR: could not insert child"

homeDir :: Dir
homeDir = Folder "/" []

moveHome :: (Dir, Crumbs) -> (Dir, [Dir])
moveHome (Folder nm dr, cs) = if nm == "/" then (Folder nm dr, cs) else moveHome $ goUp (Folder nm dr, cs)
moveHome _ = error "ERROR: parseCommand - moveHome - Pattern should not exist"


parseLine :: [Char] -> (Dir, Crumbs) -> (Dir, Crumbs)
parseLine xs
    | head xs == '$' = parseCommand $ drop 2 xs --deleting '$ ' from command
    | otherwise = parseFs xs --parse File or Directory

parseCommand :: [Char] -> (Dir, Crumbs) -> (Dir, Crumbs)
parseCommand cmd (dir, cs)
    | cmd == "cd /" = moveHome (dir, cs)
    | cmd == "cd .." = goUp (dir, cs)
    | cmd == "ls" = (dir, cs)
    | otherwise = goDown (drop 3 cmd) (dir, cs)

parseFs :: [Char] -> (Dir, Crumbs) -> (Dir, Crumbs)
parseFs cmd (dir, cs)
    | head cmd == 'd' = (insertChild (Folder (drop 4 cmd) []) dir, cs) -- a folder was read
    | isDigit $ head cmd = (insertChild (File (num, name)) dir, cs) -- a file was read
    | otherwise = error "ERROR: could not parse File/Folder"
    where num = read $ head (words cmd)
          name = last (words cmd)


applyRec :: [[Char]] -> (Dir, Crumbs) -> (Dir, Crumbs)
applyRec xs dc = foldl (flip parseLine) dc xs

buildDirTree :: [Char] -> (Dir, [Dir])
buildDirTree xs = moveHome $ applyRec (drop 1 $ lines xs) (homeDir, [])

getDir (dir, cs) = dir

determineDirSize (Folder name dirs) = sum $ map determineDirSize dirs
determineDirSize (File (num, name)) = num

dirSizeForAll :: Dir -> [Dir] -> [(String, Int)]
dirSizeForAll (File (num, name)) (x:xs) = dirSizeForAll x xs
dirSizeForAll (File (num, name)) [] = []
dirSizeForAll (Folder name []) _ = []
dirSizeForAll (Folder name (d:dirs))  [] = (name, determineDirSize (Folder name (d:dirs))) : dirSizeForAll d dirs
dirSizeForAll (Folder name dirs)  (x:xs) = (name, determineDirSize (Folder name dirs)) : dirSizeForAll x (xs++dirs)

under100 :: (Ord a1, Num a1) => (a2, a1) -> Bool
under100 (name, num)= num < 100000


solve xs = sum $ map snd $ filter under100 $ dirSizeForAll (getDir $ buildDirTree xs) []
solve2 xs = minimum $ filter (>(30000000 - (70000000 - maximum sizes))) sizes
    where sizes = map snd $ dirSizeForAll (getDir $ moveHome $ buildDirTree xs) []

main = do
    input <- getContents
    print $ solve input
    print $ solve2 input
