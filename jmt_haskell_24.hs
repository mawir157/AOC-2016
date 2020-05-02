import Data.List
import Data.Maybe

import Data.Set

type Point = (Int, Int)

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
neighbours :: Point -> [Point]
neighbours (x,y) = [(x-1, y), (x+1, y), (x, y-1) , (x, y+1)]


bfSearch :: [Point] -> Point -> [Point] -> Int
bfSearch stack target here
  | any (== target) here = 0
  | otherwise            = 1 + bfSearch stack' target goodNbrs
  where stack' = stack \\ here
        nbrs = concat $ map (neighbours) here
        goodNbrs = filter (\x -> elem x stack) nbrs

getPos :: [(Char, Point)] -> Char -> Point
getPos xs c = snd. fromJust $ find (\x -> fst x == c) xs

distances :: [(Char, Point)] -> [Point]

main :: IO()
main = do
  f <- readFile "input_24.txt"
  let s = parseInput 0 $ lines f
  -- let test = ["###########",
  --             "#0.1.....2#",
  --             "#.#######.#",
  --             "#4.......3#",
  --             "###########"]

  -- let s = parseInput 0 test
  -- putStrLn $ show s
  let open = map (\(_,p) -> p) s
  putStrLn $ show open
  let nodes = filter (\(c,p) -> c /= '.') s
  putStrLn $ show nodes

  putStrLn "Sanity check"
  putStr "Distance from 0 to 1 = "
  putStrLn . show $ bfSearch open (getPos nodes '1') [getPos nodes '0']
  putStr "Distance from 0 to 2 = "
  putStrLn . show $ bfSearch open (getPos nodes '2') [getPos nodes '0']
  putStr "Distance from 0 to 3 = "
  putStrLn . show $ bfSearch open (getPos nodes '3') [getPos nodes '0']
  putStr "Distance from 0 to 4 = "
  putStrLn . show $ bfSearch open (getPos nodes '4') [getPos nodes '0']
 
  putStr "Distance from 1 to 2 = "
  putStrLn . show $ bfSearch open (getPos nodes '2') [getPos nodes '1']
  putStr "Distance from 1 to 3 = "
  putStrLn . show $ bfSearch open (getPos nodes '3') [getPos nodes '1']
  putStr "Distance from 1 to 4 = "
  putStrLn . show $ bfSearch open (getPos nodes '4') [getPos nodes '1']

  putStr "Distance from 2 to 3 = "
  putStrLn . show $ bfSearch open (getPos nodes '3') [getPos nodes '2']
  putStr "Distance from 2 to 4 = "
  putStrLn . show $ bfSearch open (getPos nodes '4') [getPos nodes '2']

  putStr "Distance from 3 to 4 = "
  putStrLn . show $ bfSearch open (getPos nodes '4') [getPos nodes '3']