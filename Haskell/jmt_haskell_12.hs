import Data.List
import Data.List.Split

data Com = CPY | INC | DEC | JNZ deriving (Show, Eq)

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

run :: ([Reg], Int) -> [Ins] -> ([Reg], Int)
run (r, i) ins
  | i >= length ins = (r, i)
  | fst' app == JNZ = run (r, i') ins
  | otherwise       = run (r', i + 1) ins
  where app = (ins!!i)
        r' = applyIns r app
        x  = getValue r (snd' app)
        y  = fromInteger $ getValue r (thd' app)
        i' = if' (x /= 0) (i + y) (i + 1)

parseLine :: String -> Ins
parseLine s
  | c == "cpy" = (CPY,r',i')
  | c == "inc" = (INC,r',nullRegID)
  | c == "dec" = (DEC,r',nullRegID)
  | c == "jnz" = (JNZ,r',i')
  | otherwise = error ("Bad parse: " ++ s)
  where ss = (splitOn " " s)
        c = ss!!0
        r = ss!!1
        i = ss!!2
        rb = (head r) `elem` "abcd"
        r' = if' (rb) ((head r, 0, rb)) (('0', read r :: Integer, rb))
        ib = (head i) `elem` "abcd"
        i' = if' (ib) ((head i, 0, ib)) (('0', read i :: Integer, ib))

main :: IO()
main = do
  f <- readFile "../input/input12.txt"
  let l = map (parseLine) $ lines f

  putStr "Part 1: "
  let m = run (machine, 0) l
  putStrLn . show $ getValue (fst m) ('a', 1, True)

  putStr "Part 2: "
  let machine2 = setValue machine ('c', 1)
  let m = run (machine2, 0) l
  putStrLn . show $ getValue (fst m) ('a', 1, True)