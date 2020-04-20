import Data.List
import Data.List.Split

type Range = (Integer, Integer)

sortFirst :: [Range] -> [Range]
sortFirst xs =  sortBy (\(x,_) (x',_) -> compare x x') xs

sortWidth :: [Range] -> [Range]
sortWidth xs = sortBy (\(x,y) (x',y') -> compare (y-x) (y'-x')) xs

parseInput :: String -> Range
parseInput s = (read l :: Integer, read r :: Integer)
  where [l, r] = (splitOn "-" s)

redundant :: Range -> Range -> Bool
redundant (x,y) (x',y') = bx && by
  where bx = (x' <= x)
        by = (y <= y')

overlap :: Range -> Range -> Bool
overlap (x,y) (x',y') = bx || by
  where bx = (x' <= x) && (x <= y' + 1)
        by = (x' <= y + 1) && (y <= y')

combine :: Range -> Range -> Range
combine (x,y) (x',y')
  | overlap (x,y) (x',y') = (min x x', max y y')
  | otherwise             = (x,y)

combine' :: [Range] -> [Range]
combine' [] = []
combine' [x] = [x]
combine' (x:xs) = [(foldl (combine) x xs)] ++ (combine' xs)

redundant' :: [Range] -> [Range]
redundant' [] = []
redundant' [x] = [x]
redundant' (x:xs)
  | (or inside) = (redundant' xs)
  | otherwise  = [x] ++ (redundant' xs)
  where inside = map (redundant x) xs

reduceRange :: [Integer] -> Range -> [Integer]
reduceRange x (l,r) = [ t | t <- x, not (l <= t && t <= r)]

findGaps :: [Range] -> [Range]
findGaps [x] = []
findGaps (x:y:xs) = [(snd x + 1, fst y - 1)] ++ findGaps (y:xs)

main :: IO()
main = do
  f <- readFile "input_20.txt"
  let s = map (parseInput) $ lines f
  putStr "Part 1: "
  let t = combine' $ sortFirst s
  let t' = sortWidth t
  let u = sortFirst . redundant' $ sortWidth t
  putStrLn $ show ((snd $ head u) + 1)
  putStr "Part 2: "
  let g = findGaps u
  putStrLn . show . sum $ map (\(x,y) -> y - x + 1) g
