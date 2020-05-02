import Data.List
import Data.List.Split

type Pos = (Int, Int)
type Disc = (Integer, Integer, Bool)
type Node = (Pos, Disc)

-- Filesystem              Size  Used  Avail  Use%
-- /dev/grid/node-x0-y0     94T   73T    21T   77%
parseLine :: String -> Node
parseLine s = ((xs, ys),(used, avail, False))
  where f = filter (/= "") $ splitOn " " s
        ss = splitOn "-" (f!!0)
        xs = read (tail $ ss!!1) :: Int
        ys = read (tail $ ss!!2) :: Int
        used  = read (init (f!!2)) :: Integer
        avail = read (init (f!!3)) :: Integer

toPrintable :: String -> Char
toPrintable s
  | xs == 0 && ys == 0 = 'A'
  | xs == 30 && ys == 0 = 'B'
  | used < 10 = ' '
  | used < 200 = '+'
  | otherwise = '#'
  where f = filter (/= "") $ splitOn " " s
        ss = splitOn "-" (f!!0)
        xs = read (tail $ ss!!1) :: Int
        ys = read (tail $ ss!!2) :: Int
        used  = read (init (f!!2)) :: Integer
        avail = read (init (f!!3)) :: Integer

viable :: Node -> Node -> Bool
viable (_, (used, _, _)) (_, (_, avail, _))
  | used == 0 = False
  | otherwise = (used <= avail)
--------------------------------------------------------------------------------
-- there are three classes of nodes
-- Empty node - the node with no data Exactly on of these
-- Wall nodes - nodes with ~500T of data. These cannot be moved and form a wall
--              see toPrintable from a visualization
-- Open nodes - the rest, we can move exchange these with the empty node to move
--              hole
-- In our case the hole is at (13, 17)
-- The wall runs from (5,15) to (31,15)
-- The data is at (31, 0) and we want tp get it to (0,0), 30 moves up.
-- To move the hole to (30,0) takes 61 steps (9 up,27 left, 25 down)
-- Then to raise the data by one point takes 5 moves, move the data up one, then
-- take 4 steps to move the hole back above the data
-- We need to repeat this 30 time. But on the last time we don't need to move
-- the hole. So the total number of moves is:
-- 61 + 29*5 + 1 = 207
--------------------------------------------------------------------------------
main :: IO()
main = do
  f <- readFile "input_22.txt"
  let i = map (parseLine) . drop 2 $ lines f

  putStr "Part 1: "
  let t = [ (n1, n2) | n1 <- i, n2 <- i, n1 /= n2, viable n1 n2]
  putStrLn . show $ length t

  putStr "Part 2: "
  -- Uncomment to visualised the (trivial) solution
  -- let p = chunksOf 31 . map (toPrintable) . drop 2 $ lines f
  -- putStrLn ""
  -- mapM_ print p
  putStrLn $ show 207


