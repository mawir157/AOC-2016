import Data.List
import Data.List.Split
import qualified Data.Set as Set

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

data Floor = F1 | F2 | F3 | F4 deriving (Eq, Show, Ord)
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

update :: Facility -> Floor -> Set.Set Facility
update (from:cs) to
  | length ind == 0 = Set.empty
  | otherwise       = goodNhbrs
  where ind = elemIndices from cs
        mv = Set.fromList $ movable ind
        nhbrs = Set.map (replaceMult to cs) mv
        nhbrs' = Set.map (\x -> [to] ++ x) nhbrs
        goodNhbrs = Set.filter (not.facBad) nhbrs'


neighbours ::  Facility -> Set.Set Facility
neighbours f
  | head f == F1 = update f F2
  | head f == F2 = Set.union (update f F1) (update f F3)
  | head f == F3 = Set.union (update f F2) (update f F4)
  | head f == F4 = update f F3  

bfSearch :: Set.Set Facility -> Set.Set Facility -> Int
bfSearch seen here
  | any (facEnd F4) here = 0
  | otherwise            = 1 + bfSearch seen' goodNbrs
  where seen' = Set.union here seen
        nbrs = Set.unions $ Set.map (neighbours) here
        goodNbrs = Set.filter (\x -> Set.notMember x seen) nbrs

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

-- 4^11 states (41 94 304)
-- 4^15 states (1 073 741 824)

  let input1 = [F1,F1,F1,F1,F2,F1,F2,F3,F3,F3,F3]
  let input2 = [F1,F1,F1,F1,F2,F1,F2,F3,F3,F3,F3,F1,F1,F1,F1]

  putStr "Part 1: "
  let seen = Set.fromList []
  let here = Set.fromList [input1]
  let k = bfSearch seen here
  putStrLn $ show k

  -- Absurdly Slow
  putStr "Part 2: "
  let here = Set.fromList [input2]
  let k = bfSearch seen here
  putStrLn $ show k
