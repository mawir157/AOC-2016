import Data.List
import Data.List.Split

import Debug.Trace

sublists  _     0 = [[]]
sublists  []    _ = []
sublists (x:xs) n = sublists xs n ++ map (x:) (sublists xs $ n - 1)

movable xs = (sublists xs 2) ++ chunksOf 1 xs

-- Elevator must contain at least one component to move
-- Elevator can carry at most two components at a time
-- Elevator stops at each floor

-- Bad - Mircochip A + RTG B without RTG A
-- BAD .. Am Bg ..
-- OK  Ag Am Bg ..
-- OK  .. Am .. Bm.

data Mode = CHIP | GEN deriving (Eq, Show)
type Component = (Char, Mode)

type Floor = [Component]
type Facility = ([Floor], Int)

isChip :: Component -> Bool
isChip (c, m) = m == CHIP

isGen :: Component -> Bool
isGen (c, m) = m == GEN

fEmpty :: Floor -> Bool
fEmpty f = length f == 0 

fBad :: Floor -> Bool
fBad f
  |(length isolated == 0) = False
  | otherwise             = (length (gens) /= 0)
  where chips = map (fst) $ filter (isChip) f
        gens =  map (fst) $ filter (isGen) f
        isolated = (chips \\ gens)

facGood :: Facility -> Bool
facGood (fs,_) = all (not.fBad) fs

mvFloor :: [Floor] -> Int -> Int -> [Component] -> [Floor]
mvFloor fs from to comps
  | from == 0 && to == 1 = [f] ++ [t] ++ [fs!!2] ++ [fs!!3]
  | from == 1 && to == 2 = [fs!!0] ++ [f] ++ [t] ++ [fs!!3]
  | from == 2 && to == 3 = [fs!!0] ++ [fs!!1] ++ [f] ++ [t]
  | from == 3 && to == 2 = [fs!!0] ++ [fs!!1] ++ [t] ++ [f]
  | from == 2 && to == 1 = [fs!!0] ++ [t] ++ [f] ++ [fs!!3]
  | from == 1 && to == 0 = [t] ++ [f] ++ [fs!!2] ++ [fs!!3]
  where f = (fs!!from) \\ comps
        t = (fs!!to) ++ comps

allLegal :: Facility -> [Facility]
allLegal (fs, e)
  | e == 0 = nub $ filter (facGood) (zip mv01 (repeat 1))
  | e == 1 = nub $ filter (facGood) (zip mv12 (repeat 2) ++ zip mv10 (repeat 0)) 
  | e == 2 = nub $ filter (facGood) (zip mv21 (repeat 1) ++ zip mv23 (repeat 3))
  | e == 3 = nub $ filter (facGood) (zip mv32 (repeat 2))
  where mv = movable (fs!!e)
        mv01 = map (mvFloor fs 0 1) mv
        mv12 = map (mvFloor fs 1 2) mv
        mv23 = map (mvFloor fs 2 3) mv
        mv32 = map (mvFloor fs 3 2) mv
        mv21 = map (mvFloor fs 2 1) mv
        mv10 = map (mvFloor fs 1 0) mv

run :: Int -> Facility -> Int
run c f 
  | end f             = c
  | c > 1000          = 1000000
  | length legal == 0 = 1000000
  | otherwise = minimum $ map (run c') legal
  where legal = allLegal f
        c' = traceShowId $ c+1

end :: Facility -> Bool
end (floors, elevator) = bf && be
  where bf = all (fEmpty) $ init floors
        be = (elevator == 3)

main :: IO()
main = do
-- F4 .  .. .. .. .. 
-- F3 .  .. .. Lg .. 
-- F2 .  Hg .. .. .. 
-- F1 E  .. Hm .. Lm 

-- 4^5 states (1024)
  let f = ([[('H', CHIP),('L', CHIP)],
            [('H', GEN)],
            [('L', GEN)],
            []], 0)
-- F4 . .. .. .. .. .. .. .. .. .. ..
-- F3 . .. .. .. .. .. .. Qg Qm Rg Rm
-- F2 . .. .. .. Pm .. Sm .. .. .. ..
-- F1 E Tg Tm Pg .. Sg .. .. .. .. ..

-- 4^11 states (4194304)

  let f = ([[('T', GEN),('T', CHIP),('P', GEN),('S', GEN)],
            [('P', CHIP),('S', CHIP)],
            [('Q', GEN),('Q', CHIP),('R', GEN),('R', CHIP)],
            []], 0)


  let k = map (fst) $ allLegal f
  putStrLn $ show k

  putStr "Part 1: "
  let t = run 0 f
  putStrLn $ show t

  -- putStr "Part 2: "
  -- let t = run (f, 0)
  -- putStrLn $ show t