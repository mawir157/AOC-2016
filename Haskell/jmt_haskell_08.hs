data Inst = RECT | ROTR | ROTC deriving (Eq, Show)
type Command = (Inst, Int, Int)

grid :: [[Char]]
grid = replicate 6 $ replicate 50 ' '

-- rect 3x3
toRectCommand :: String -> Command
toRectCommand ss = (RECT, x, y)
  where x = read (takeWhile (\x->x/='x') . drop 1 $ dropWhile(\x->x/=' ') ss) :: Int
        y = read (drop 1 $ dropWhile (\x->x/='x') ss) :: Int

-- rotate row y=0 by 5
toRotCommand :: String -> Command
toRotCommand ss
  | k == 'r' = (ROTR, rc, d)
  | k == 'c' = (ROTC, rc, d)
  where k = ss!!7
        rc = read (takeWhile (\x->x/=' ') . drop 1 $ dropWhile(\x->x/='=') ss) :: Int
        d = read (drop 3 $ dropWhile(\x->x/='b') ss) :: Int

parseInput :: String -> Command
parseInput ss
  | ss!!1 == 'e' = toRectCommand ss
  | ss!!1 == 'o' = toRotCommand ss

setN :: Int -> [Char] -> [Char]
setN n xs = (replicate n '#') ++ (drop n xs)

rect :: [[Char]] -> Int -> Int -> [[Char]]
rect g x y = (map (setN x) (take y g)) ++ (drop y g)

rotr' :: Int -> [Char] -> [Char]
rotr' v xs = (drop (l - v) xs) ++ (take (l - v) xs)
  where l = length xs

rotr :: [[Char]] -> Int -> Int -> [[Char]]
rotr g r v = (take r g) ++ [(rotr' v (g!!(r)))] ++ (drop (r + 1) g)

rotc' :: Int -> [Char] -> [[Char]] -> [[Char]]
rotc' n [] _ = []
rotc' n _ [] = []
rotc' n (c:cs) (x:xs) = [newrow] ++ rotc' n cs xs
  where newrow = (take n x) ++ [c] ++ (drop (n+1) x)

rotc :: [[Char]] -> Int -> Int -> [[Char]]
rotc g c v = rotc' c col' g
  where col = map (\x-> x!!c) g
        col' = rotr' v col 

applyInstr :: [[Char]] -> Command -> [[Char]]
applyInstr g (c, x, y)
  | c == RECT = rect g x y
  | c == ROTR = rotr g x y
  | c == ROTC = rotc g x y

rowSum :: [Char] -> Integer
rowSum [] = 0
rowSum (x:xs)
  | x == '#'  = 1 + rowSum xs
  | otherwise = rowSum xs

main = do 
  f <- readFile "../input/input08.txt"
  let i = map parseInput $ lines f
  let grid' = foldl (applyInstr) grid i

  putStr "Part 1: "
  putStrLn . show . sum $ map (rowSum) grid'
  putStrLn "Part 2: "
  mapM_ putStrLn $ grid'
