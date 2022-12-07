type Name = String
type Data = (Int, Name)
data Dir = File Data | Folder Name [Dir] deriving (Show)

file :: Data
file = (3, "asdf")

dir :: Dir
dir = Folder "/" [File file]

--insertElem :: File -> Dir -> Dir
insert elem dir = elem
--parseLine xs 
--    | head xs == '$' = parseCommand $ drop 2 xs --deleting '$ ' from command
--    | otherwise = parseFs xs --parse File or Directory

solve = lines

main = do
    input <- getContents
    print $ solve input