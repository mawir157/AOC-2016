import Data.List.Split
import Data.List
import qualified Data.Set as Set

if' True  x _ = x
if' False _ x = x

firstMatch :: (Eq a) => [a] -> a
firstMatch xs = fst . head $ dropWhile (\x -> not (snd x)) xs'
  where xs' = zip xs $ map (repeatInFirstN xs) [0,1..(length xs - 1)]

repeatInFirstN :: (Eq a) => [a] -> Int -> Bool
repeatInFirstN xs n = x' `elem` xs'
  where xs' = take n xs
        x' = head $ drop n xs

data Dir = N | E | S | W deriving (Show, Eq)

parseCom :: String -> (Char, Int)
parseCom ss = (head ss, (read (tail ss) :: Int))

parseInput :: String -> [(Char, Int)]
parseInput s = map (parseCom) $ splitOn ", " s

updateDir :: Dir -> Char -> Dir
updateDir d c
  | d == N = if' (c == 'R') E W
  | d == E = if' (c == 'R') S N
  | d == S = if' (c == 'R') W E
  | d == W = if' (c == 'R') N S

updatePos :: (Int, Int) -> Dir -> Int -> (Int, Int)
updatePos (x, y) d p
  | d == N = (x, y + p)
  | d == E = (x + p, y)
  | d == S = (x, y - p)
  | d == W = (x - p, y)

move :: (Dir, (Int, Int)) -> (Char, Int) -> (Dir, (Int, Int))
move (d, (x, y)) (c, p) = (d', (x', y'))
  where d' = updateDir d c
        (x', y') = updatePos (x, y) d' p

move' :: (Dir, [(Int, Int)]) -> (Char, Int) -> (Dir, [(Int, Int)])
move' (d, xs) (c, p) = (d', xs ++ xs')
  where d' = updateDir d c
        xs' = map (updatePos (last xs) d') [1,2..p]

dist :: (Int, Int) -> Int
dist (x,y) = abs x + abs y

main = do 
  f <- readFile "input_01.txt"
  let i = parseInput f
  putStr "part 1: "
  let (d, (x,y)) = foldl move (N, (0,0)) i
  putStrLn . show $ dist (x,y)
  putStr "part 2: "
  let (e, xs) = foldl move' (N, [(0,0)]) i
  putStrLn . show . dist $ firstMatch xs
