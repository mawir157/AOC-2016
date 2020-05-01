import Data.List
import Data.List.Split
import qualified Data.Sequence as Seq

import Debug.Trace

oddElts :: [a] -> [a]
oddElts [] = []
oddElts [x] = [x]
oddElts (x:y:xs) = [x] ++ oddElts xs

sublists  _     0 = [[]]
sublists  []    _ = []
sublists (x:xs) n = sublists xs n ++ map (x:) (sublists xs $ n - 1)

movable xs = (sublists xs 2) ++ chunksOf 1 xs

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace n v (x:xs)
   | n == 0    = v:xs
   | otherwise = x:replace (n-1) v xs

replaceMult ::  a -> [a] -> [Int] ->[a]
replaceMult _ xs []     = xs
replaceMult v xs (n:ns) = replaceMult v (replace n v xs) ns

-- Elevator must contain at least one component to move
-- Elevator can carry at most two components at a time
-- Elevator stops at each floor

-- Bad - Mircochip A + RTG B without RTG A
-- BAD .. Am Bg ..
-- OK  Ag Am Bg ..
-- OK  .. Am .. Bm.

-- F4 . .. .. .. .. .. .. .. .. .. ..
-- F3 . .. .. .. .. .. .. Qg Qm Rg Rm
-- F2 . .. .. .. Pm .. Sm .. .. .. ..
-- F1 E Tg Tm Pg .. Sg .. .. .. .. ..

-- == F1 F1 F1 F1 F2 F1 F2 F3 F3 F3 F3

data Floor = F1 | F2 | F3 | F4 deriving (Eq, Show)
type Facility = [Floor]

chips :: Facility -> [Floor]
chips f = drop 1 $ oddElts f

gens :: Facility -> [Floor]
gens (e:cs) = oddElts cs

facBad :: Facility -> Bool
facBad f
  | length i == 0 = False
  | otherwise     = length badChips > 0
  where gs = gens f
        i = filter (\[g,m] -> g /= m) (chunksOf 2 $ tail f)
        isoChips = map (!!1) i
        badChips = filter (\x -> x `elem` gs) isoChips

facEnd :: Floor -> Facility -> Bool
facEnd f fac = all (== f) fac

update :: Facility -> Floor -> [Facility]
update (from:cs) to
  | length ind == 0 = []
  | otherwise       = goodNhbrs
  where ind = elemIndices from cs
        mv = movable ind
        nhbrs = map (replaceMult to cs) mv
        nhbrs' = map (\x -> [to] ++ x) nhbrs
        goodNhbrs = filter (not.facBad) nhbrs'


neighbours ::  Facility -> [Facility]
neighbours f
  | head f == F1 = update f F2
  | head f == F2 = nub ((update f F1) ++ (update f F3))
  | head f == F3 = nub ((update f F2) ++ (update f F4))
  | head f == F4 = update f F3  

search :: [Facility] -> Int -> Facility -> Int
search seen dist here 
  | facEnd F4 here       = dist 
  | length goodNbrs == 0 = 10000000
  | otherwise            = minimum $ map (search seen' (dist+1)) goodNbrs 
  where nbrs = neighbours here
        goodNbrs = filter (\x -> not (x `elem` seen)) nbrs
        seen' = [here] ++ seen

bfSearch :: [Facility] -> Int -> [Facility] -> Int
bfSearch seen dist here
  | any (facEnd F4) here = dist
  | otherwise            = bfSearch seen' (dist') goodNbrs
  where dist' = traceShowId $ dist + 1
        seen' = here ++ seen
        nbrs = concat $ map (neighbours) here
        goodNbrs = filter (\x -> not (x `elem` seen)) nbrs

main :: IO()
main = do
-- F4 .  .. .. .. .. 
-- F3 .  .. .. Lg .. 
-- F2 .  Hg .. .. .. 
-- F1 E  .. Hm .. Lm 

  let test = [F1, F2, F1, F3, F1] 

-- 4^5 states (1024)

-- F4 . .. .. .. .. .. .. .. .. .. ..
-- F3 . .. .. .. .. .. .. Qg Qm Rg Rm
-- F2 . .. .. .. Pm .. Sm .. .. .. ..
-- F1 E Tg Tm Pg .. Sg .. .. .. .. ..

-- 4^11 states (4194304)

  let input = [F1,F1,F1,F1,F2,F1,F2,F3,F3,F3,F3]

  putStr "Part 1: "
  -- let k = bfSearch [] 0 [test]
  -- putStrLn $ show k

  putStr "Part 2: "
  let seen = []
  let here = [input]
  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

  let seen' = (here ++ seen)
  let seen = seen'
  let nbrs = concat $ map (neighbours) here
  let here = filter (\x -> not (x `elem` seen)) nbrs
  -- putStrLn $ show seen
  -- putStrLn $ show here
  putStrLn . show $ any (facEnd F4) here
  putStrLn ""

-- [F1, F2, F1, F3, F1]
-- [F2, F2, F2, F3, F1]
-- [F3, F3, F3, F3, F1]
-- [F2, F3, F2, F3, F1]
-- [F1, F3, F1, F3, F1]
-- [F2, F3, F2, F3, F2]
-- [F3, F3, F3, F3, F3]
-- [F4, F3, F4, F3, F4]
-- [F3, F3, F3, F3, F4]
-- [F4, F4, F3, F4, F4]
-- [F3, F4, F3, F4, F3]
-- [F4, F4, F4, F4, F4]
