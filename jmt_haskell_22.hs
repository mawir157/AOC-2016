import Data.List.Split

type Disc = (Int, Integer)
type Node = ((Int, Int), (Integer, Integer))

-- Filesystem              Size  Used  Avail  Use%
-- /dev/grid/node-x0-y0     94T   73T    21T   77%
parseLine :: String -> Node
parseLine s = ((xs, ys),(used, avail))
  where f = filter (/= "") $ splitOn " " s
        ss = splitOn "-" (f!!0)
        xs = read (tail $ ss!!1) :: Int
        ys = read (tail $ ss!!2) :: Int
        used  = read (init (f!!2)) :: Integer
        avail = read (init (f!!3)) :: Integer

viable :: Node -> Node -> Bool
viable ((_, _), (used, _)) ((_, _), (_, avail))
  | used == 0 = False
  | otherwise = (used <= avail)

main :: IO()
main = do
  f <- readFile "input_22.txt"
  let i = map (parseLine) . drop 2 $ lines f
  putStr "part 1: "
  let t = [ (n1, n2) | n1 <- i, n2 <- i, n1 /= n2, viable n1 n2]
  putStrLn . show $ length t