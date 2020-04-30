import Data.List
import Data.Maybe

type Point = (Int, Int)

if' True  x _ = x
if' False _ x = x

toBin 0 = [0]
toBin n | n `mod` 2 == 1 = [1] ++ toBin (n `div` 2)
        | n `mod` 2 == 0 = [0] ++ toBin (n `div` 2)

isOpen :: Int -> Int -> Int -> Bool
isOpen input y x = even b
  where t = (x*x + 3*x + 2*x*y + y + y*y) + input
        b = sum $ toBin t

spacePairs :: Int -> (Int, Int) -> [Point]
spacePairs input (lx, ly) = 
  [ (x,y) | x <- [0..lx], y <- [0..ly], isOpen input y x]

getAdjs :: Point -> [Point]
getAdjs (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

dfsSearchTo :: [Point] -> Point -> Int -> Point -> Int
dfsSearchTo stack there dist here
  | here == there    = dist 
  | length nbrs == 0 = 10000000
  | otherwise        = minimum $ map (dfsSearchTo stack' there (dist+1)) nbrs 
  where stack' = filter (\x -> x /= here) stack
        nbrs = filter (\x -> x `elem` stack') $ getAdjs here

bfsSearch :: Int -> [Point] -> (Point, Int) -> [(Point,Int)]
bfsSearch lim stack (p,d)
  | d == lim         = [(p,d)]
  | length nbrs == 0 = [(p,d)]
  | otherwise        = [(p,d)] ++ (concat $ map (bfsSearch lim stack') nbrs')
  where stack' = filter (\x -> x /= p) stack
        nbrs = filter (\x -> x `elem` stack') $ getAdjs p
        nbrs' = zip nbrs $ repeat (d+1)

main :: IO()
main = do
  let i = 1352
  let pairs = spacePairs i (40, 40)
  let target = (31,39)
  let start = (1,1)

  putStr "Part 1: "
  let t = dfsSearchTo pairs target 0 start
  putStrLn $ show t

  putStr "Part 2: "
  let pairs = spacePairs i (51, 51)
  let tt = bfsSearch 50 pairs (start, 0)
  let rr = nubBy (\x y-> fst x == fst y) tt
  putStrLn . show $ length rr
