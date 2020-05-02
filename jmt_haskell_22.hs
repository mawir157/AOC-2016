import Data.List
import Data.List.Split

import Debug.Trace

import qualified Data.Set as Set

type Disc = (Int, Integer)
type Node = ((Int, Int), (Integer, Integer, Bool))

type Cpu = [Node]

nodeUsed    (_,(d,_,_)) = d
nodeAvail   (_,(_,a,_)) = a
nodePayload (_,(_,_,b)) = b

clearData :: Node -> Node
clearData (p,(d,a,b)) = (p,(0,a+d,False))

addData :: Node -> Bool -> Integer -> Node
addData (p,(d,a,b)) q n
  | n + d > a     = error "Not enough space"
  | otherwise = (p,(n+d,a-n,q))

setTrue :: Node -> Node
setTrue (p,(d,a,b)) = (p,(d,a,True))

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

viable :: Node -> Node -> Bool
viable (_, (used, _, _)) (_, (_, avail, _))
  | used == 0 = False
  | otherwise = (used <= avail)
--------------------------------------------------------------------------------
canMove :: Node -> Node -> Bool
canMove n1 n2 = (viable n1 n2) && (adjacent n1 n2)

adjacent :: Node -> Node -> Bool
adjacent ((x,y),_) ((u,v),_)
  | abs (x - u) == 1 && y == v = True
  | abs (y - v) == 1 && x == u = True
  | otherwise                  = False

moveData :: Cpu -> (Node, Node) -> Cpu
moveData ns (from, to)
  | nodeUsed from + nodeUsed to > nodeAvail to = error (show (from, to))
  | otherwise = sort r
  where ns' = ns \\ [from, to]
        d = nodeUsed from
        b = nodePayload from
        r = ns' ++ [clearData from] ++ [addData to b d]

allMoves :: Cpu -> Set.Set (Node, Node)
allMoves ns = mvs
  where mvs = Set.fromList [ (n1, n2) | n1 <- ns, n2 <- ns, n1 /= n2, canMove n1 n2]

neighbours :: Cpu -> Set.Set Cpu
neighbours c = Set.map (moveData c) mvs
  where mvs = allMoves c

setTarget :: Cpu -> (Int, Int) -> Cpu
setTarget ns p = ns' ++ n'
  where (ns', n) = partition (\x -> fst x /= p) ns
        n' = map (setTrue) n

end :: Cpu -> Bool
end [] = False
end (n:ns)
  | fst n == (0,0) = nodePayload n
  | otherwise      = end ns

bfSearch :: Set.Set Cpu -> Set.Set Cpu -> Int
bfSearch seen here
  | any (end) here = 0
  | otherwise      = 1 + bfSearch seen' goodNbrs
  where seen' = Set.union here seen
        nbrs = Set.unions $ Set.map (neighbours) here
        goodNbrs = Set.filter (\x -> Set.notMember x seen) nbrs
        !r = traceShowId $ (Set.size seen', Set.size nbrs, Set.size goodNbrs)

-- Filesystem              Size  Used  Avail  Use%
-- /dev/grid/node-x0-y0     94T   73T    21T   77%
-- /dev/grid/node-x30-y0    88T   67T    21T   76%

-- endState node 0 0 67 27

main :: IO()
main = do
  f <- readFile "input_22.txt"
  let i = map (parseLine) . drop 2 $ lines f

  -- let test = ["/dev/grid/node-x0-y0   10T    8T     2T   80%",
  --             "/dev/grid/node-x0-y1   11T    6T     5T   54%",
  --             "/dev/grid/node-x0-y2   32T   28T     4T   87%",
  --             "/dev/grid/node-x1-y0    9T    7T     2T   77%",
  --             "/dev/grid/node-x1-y1    8T    0T     8T    0%",
  --             "/dev/grid/node-x1-y2   11T    7T     4T   63%",
  --             "/dev/grid/node-x2-y0   10T    6T     4T   60%",
  --             "/dev/grid/node-x2-y1    9T    8T     1T   88%",
  --             "/dev/grid/node-x2-y2    9T    6T     3T   66%"]
  -- let i = map (parseLine) test

  putStr "Part 1: "
  let t = [ (n1, n2) | n1 <- i, n2 <- i, n1 /= n2, viable n1 n2]
  putStrLn . show $ length t

  putStr "Part 2: "
  let k = Set.fromList $ [setTarget i (30,0)]
  let s = bfSearch (Set.empty) k
  putStrLn $ show s
  putStrLn "Goodbye World!"
