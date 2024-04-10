import Data.List
import Data.Maybe

import qualified Data.Set as Set

type Point = (Int, Int)
type Graph = [((Char, Char), Integer)]

if' True  x _ = x
if' False _ x = x

parseInput :: Int -> [String] -> [(Char, Point)]
parseInput _ [] = []
parseInput r (x:xs) = (parseLine r 0 x) ++ parseInput (r+1) xs

parseLine :: Int -> Int -> String -> [(Char, Point)]
parseLine _ _ [] = []
parseLine r c (x:xs)
  | x == '#'  = (parseLine r (c+1) xs)
  | otherwise = [(x,(c,r))] ++ (parseLine r (c+1) xs)

-- we do not need to be careful here  since the grid is 
-- surrounding by a solid wall
neighbours :: Point -> Set.Set Point
neighbours (x,y) = Set.fromList [(x-1, y), (x+1, y), (x, y-1) , (x, y+1)]


bfSearch :: Set.Set Point -> Point -> Set.Set Point -> Integer
bfSearch stack target here
  | any (== target) here = 0
  | otherwise            = 1 + bfSearch stack' target goodNbrs
  where stack' = stack Set.\\ here
        nbrs = Set.unions $ Set.map (neighbours) here
        goodNbrs = Set.filter (\x -> Set.member x stack) nbrs

getPos :: [(Char, Point)] -> Char -> Point
getPos xs c = snd. fromJust $ find (\x -> fst x == c) xs

distance :: [(Char, Point)] -> Set.Set Point -> (Char, Char) -> Integer
distance nodes open (from, to) = bfSearch open p1 $ Set.fromList [p0]
  where  p0 = getPos nodes from
         p1 = getPos nodes to

tupleMatch :: (Char, Char) -> (Char, Char) -> Bool
tupleMatch (x,y) (a,b) = (x==a && y==b) || (x==b && y==a)

graphDist :: Graph -> (Char, Char) -> Integer
graphDist edges (from, to) = d
  where (_,d) = fromJust $ find(\(e,_) -> tupleMatch e (from, to)) edges

routeLength :: Graph -> String -> Integer
routeLength es ss
  | length ss == 2 = graphDist es (f,t)
  | length ss > 2  = graphDist es (f,t) + (routeLength es (drop 1 ss))
  where f = ss!!0
        t = ss!!1

main :: IO()
main = do
  f <- readFile "../input/input24.txt"
  let s = parseInput 0 $ lines f

  -- let test = ["###########",
  --             "#0.1.....2#",
  --             "#.#######.#",
  --             "#4.......3#",
  --             "###########"]

  -- let s = parseInput 0 test

  -- tedious nonsense to build the graph
  let open = Set.fromList $ map (\(_,p) -> p) s
  let nodes = filter (\(c,p) -> c /= '.') s
  let labels = sort $ map (\(c,_) -> c) nodes
  let pairs = [ (x,y) | x <- labels, y <- labels, x < y]
  let dists = map (distance nodes open) pairs
  let graph = zip pairs dists
  let p1 = map (\x -> "0" ++ x) . permutations $ tail labels
  let ds = map (routeLength graph) p1

  putStr "Part 1: " 
  putStrLn $ show $ minimum ds

  let p2 = map (\x -> x ++ "0") p1
  let ds = map (routeLength graph) p2
  putStr "Part 2: " 
  putStrLn $ show $ minimum ds
