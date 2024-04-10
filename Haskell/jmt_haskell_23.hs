import Data.List
import Data.List.Split

import Debug.Trace

data Com = CPY | INC | DEC | JNZ | TGL deriving (Show, Eq)

type RegId = (Char, Integer, Bool)
fst' (c,_,_) = c
snd'  (_,i,_) = i
thd' (_,_,b) = b
nullRegID = ('0',0,False)

type Reg = (Char, Integer)
type Ins = (Com, RegId, RegId)

if' True  x _ = x
if' False _ x = x

machine = [('a', 0), ('b', 0), ('c', 0), ('d', 0)]

getValue :: [Reg] -> RegId -> Integer
getValue rs (c,i,b)
  | b         = snd . head $ dropWhile (\(q,p) -> q /= c) rs
  | otherwise = i

setValue :: [Reg] -> (Char, Integer) -> [Reg]
setValue rs (i, v) = (filter (\(q,p) -> q /= i) rs) ++ [(i, v)]
  
applyIns :: [Reg] -> Ins -> [Reg]
applyIns rs (c, r1, r2)
  | c == CPY  = setValue rs (fst' r2, v1)
  | c == INC  = setValue rs (fst' r1, v1 + 1)
  | c == DEC  = setValue rs (fst' r1, v1 - 1)
  | otherwise = error "Unknown Command"
  where v1 = getValue rs r1
        v2 = getValue rs r2

updatePointer :: Int -> Integer -> Integer -> Int
updatePointer ptr x y = if' (x /= 0) (ptr + fromIntegral y) (ptr + 1)

updateInstruction :: [Ins] -> Int -> [Ins]
updateInstruction ins n
  | n >= length ins = ins
  | com == CPY = (take n ins) ++ [(JNZ, r1, r2)] ++ (drop (n+1) ins)
  | com == INC = (take n ins) ++ [(DEC, r1, r2)] ++ (drop (n+1) ins)
  | com == DEC = (take n ins) ++ [(INC, r1, r2)] ++ (drop (n+1) ins)
  | com == JNZ = (take n ins) ++ [(CPY, r1, r2)] ++ (drop (n+1) ins)
  | com == TGL = (take n ins) ++ [(INC, r1, r2)] ++ (drop (n+1) ins)
  where (com, r1, r2) = (ins!!n)

run :: (([Reg], Int), [Ins]) -> (([Reg], Int), [Ins])
run ((r, ptr), ins)
  | ptr >= length ins = ((r, ptr), ins) -- run past the end of the program
  | com == JNZ        = run ((r, ptr'), ins) --update pointer and keep running
  | com == TGL        = run ((r, ptr + 1), ins') -- update instructions and continue
  | otherwise         = run ((r', ptr + 1), ins) -- update registries and continue
  where (com, reg1, reg2) = (ins!!ptr)
        r' = applyIns r (com, reg1, reg2)
        ptr' = updatePointer ptr (getValue r reg1) (getValue r reg2)
        ins' = updateInstruction ins (ptr + (fromIntegral (getValue r reg1)))

parseLine :: String -> Ins
parseLine s
  | c == "cpy" = (CPY,r',i')
  | c == "inc" = (INC,r',nullRegID)
  | c == "dec" = (DEC,r',nullRegID)
  | c == "jnz" = (JNZ,r',i')
  | c == "tgl" = (TGL,r',nullRegID)
  | otherwise = error ("Bad parse: " ++ s)
  where ss = (splitOn " " s)
        c = ss!!0
        r = ss!!1
        i = ss!!2
        rb = (head r) `elem` "abcd"
        r' = if' (rb) ((head r, 0, rb)) (('0', read r :: Integer, rb))
        ib = (head i) `elem` "abcd"
        i' = if' (ib) ((head i, 0, ib)) (('0', read i :: Integer, ib))

-- this was worked out by hand f(n) = n! + (96*79)
decompiled :: Integer -> Integer
decompiled n = (product [1..n]) + (96*79)

main :: IO()
main = do
  f <- readFile "../input/input23.txt"
  let l = map (parseLine) $ lines f

  putStr "Part 1: "
  let machine1 = setValue machine ('a', 6)
  let ((m, p), i) = run ((machine1, 0), l)
  putStrLn . show $ getValue m ('a', 1, True)

  putStr "Part 2: "
  putStrLn . show $ decompiled 12
