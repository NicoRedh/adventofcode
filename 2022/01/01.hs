import qualified Data.List as L
import qualified Data.Text as T
import Data.ByteString.Builder.Extra (wordHost)
enlist ch xs = [repl x | x <- xs] where repl a = if a == ch then ' ' else a
read1 xs = read xs :: Int

splitText xs = map (words . T.unpack) (T.splitOn (T.pack "  ") (T.pack $ enlist '\n' xs))
solve xs = maximum $ map (sum . map read1) (splitText xs)

last3 [] = []
last3 xs = drop (length xs - 3) xs
solve2 xs = sum $ last3 $ L.sort $ map (sum . map read1) (splitText xs)

main = do
  input <- getContents
  print $ "Part1: " ++ show (solve input)
  print $ "Part2: " ++ show (solve2 input)