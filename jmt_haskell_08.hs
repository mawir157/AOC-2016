data Inst = Rect | RotR | RotC deriving (Eq, Show)
type Command = (Inst, Integer, Integer)

grid :: [Integer]
grid = replicate (50 * 6) 0 

-- rect 3x3
toRectCommand :: String -> Command
toRectCommand ss = (Rect, x, y)
  where x = read (takeWhile (\x->x/='x') . drop 1 $ dropWhile(\x->x/=' ') ss) :: Integer
        y = read (drop 1 $ dropWhile (\x->x/='x') ss) :: Integer

-- rotate row y=0 by 5
toRotCommand :: String -> Command
toRotCommand ss
  | k == 'r' = (RotR, rc, d)
  | k == 'c' = (RotC, rc, d)
  where k = ss!!7
        rc = read (takeWhile (\x->x/=' ') . drop 1 $ dropWhile(\x->x/='=') ss) :: Integer
        d = read (drop 3 $ dropWhile(\x->x/='b') ss) :: Integer

parseInput :: String -> Command
parseInput ss
  | ss!!1 == 'e' = toRectCommand ss
  | ss!!1 == 'o' = toRotCommand ss

applyInstr :: [Integer] -> Command -> [Integer]
applyInstr g (c, x, y)

main = do 
  f <- readFile "input_08.txt"
  let i = map parseInput $ lines f
  putStrLn $ show i
  putStr "Part 1: "
  putStr "Part 2: "
