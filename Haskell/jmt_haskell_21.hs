import Data.List 
import Data.List.Split
import Data.Maybe
import Data.Char

data CMD = MOVE | REVS | SWPP | SWPL | ROTA | ROTP | ERR deriving (Eq, Show)
type Dual = (Char, Int, Bool)
type Instruction = (CMD, Int, Int)

if' True  x _ = x
if' False _ x = x

parseLine :: String -> Instruction
parseLine s
  | "move" == ss!!0     = (MOVE, read (ss!!2) :: Int, read (ss!!5) :: Int)
  | "reverse" == ss!!0  = (REVS, read (ss!!2) :: Int, read (ss!!4) :: Int)
  | "position" == ss!!1 = (SWPP, read (ss!!2) :: Int, read (ss!!5) :: Int)
  | "letter" == ss!!1   = (SWPL, ord (head $ ss!!2), ord (head $ ss!!5))
  | "based" == ss!!1    = (ROTP, ord (head $ ss!!6), 0)
  | "left" == ss!!1     = (ROTA, read (ss!!2) :: Int, 0)
  | "right" == ss!!1    = (ROTA, (-1) * (read (ss!!2) :: Int), 0)
  where ss = splitOn " " s 

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

move :: String -> Int -> Int -> String
move s x y = insertAt xc y ss
  where xc = (s!!x)
        ss = (take x s) ++ (drop (succ x) s)

rev :: String -> Int -> Int -> String
rev s x y = (take x s) ++ r ++ (drop (succ y) s)
  where r = reverse . drop (x) $ take (succ y) s;

swapPos :: String -> Int -> Int -> String
swapPos s x y = s1 ++ [s!!y'] ++ s2 ++ [s!!x'] ++ s3
  where x' = min x y
        y' = max y x
        s1 = take x' s;
        s2 = drop (succ x') (take y' s);
        s3 = drop (succ y') s

swapLet :: String -> Char -> Char -> String
swapLet s a b = swapPos s ai bi
  where ai = fromJust $ elemIndex a s
        bi = fromJust $ elemIndex b s

rotateAbs :: String -> Int -> String
rotateAbs s n = take (length s) . (drop p) $ cycle s
  where p = mod n (length s)

rotatePos :: String -> Char -> String
rotatePos s c = rotateAbs s (-n)
  where t = fromJust $ elemIndex c s
        n = t + 1 + if' (t >= 4) 1 0 

tick :: String -> Instruction -> String
tick s (c,x,y)
  | MOVE == c = move s x y
  | REVS == c = rev  s x y
  | SWPP == c = swapPos s x y
  | SWPL == c = swapLet s (chr x) (chr y)
  | ROTP == c = rotatePos s (chr x)
  | ROTA == c = rotateAbs s x

mapsTo :: [Instruction] -> String -> String -> Bool
mapsTo i target candidate = (image == target)
  where image = foldl tick candidate i

main :: IO()
main = do
  f <- readFile "../input/input21.txt"
  let i = map (parseLine) $ lines f
  putStr "part 1: "
  let iString = "abcdefgh"
  let k = foldl tick iString i 
  putStrLn $ show k
  putStr "part 2: " -- I am lazy
  let perms = permutations iString
  let s = fromJust $ find (mapsTo i "fbgdceah") perms
  putStrLn . show $ s
  -- sanity 