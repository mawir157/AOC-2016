fst' (x,_,_) = x
snd' (_,x,_) = x
thd' (_,_,x) = x

dropChar :: Char -> String ->  String
dropChar c ss = dropWhile (\x -> x == c) ss

takeNotChar :: Char -> String ->  String
takeNotChar c ss = takeWhile (\x -> x /= c) ss

dropNotChar :: Char -> String ->  String
dropNotChar c ss = dropWhile (\x -> x /= c) ss

parseInput :: String -> (Int, Int, Int)
parseInput ss = (x, y, z)
  where x = read (takeNotChar ' ' $ dropChar ' ' ss) :: Int
        y = read (takeNotChar ' ' . dropChar ' ' . dropNotChar ' ' $ dropChar ' ' ss) :: Int
        z = read (takeNotChar ' ' . dropChar ' ' . dropNotChar ' ' . dropChar ' ' . dropNotChar ' ' $ dropChar ' ' ss) :: Int

isTriangle :: (Int, Int, Int) -> Bool
isTriangle (x, y, z) = (x + y > z) && (y + z > x) && (z + x > y)

refactor :: [(Int, Int, Int)] -> [(Int, Int, Int)]
refactor [] = []
refactor x = [(fst' (x3!!0), fst' (x3!!1), fst' (x3!!2)),
              (snd' (x3!!0), snd' (x3!!1), snd' (x3!!2)),
              (thd' (x3!!0), thd' (x3!!1), thd' (x3!!2))] ++ (refactor $ drop 3 x)
  where x3 = take 3 x

main = do 
  f <- readFile "input_03.txt"
  let i = map parseInput $ lines f
  putStr "Part 1: "
  putStrLn . show $ length $ filter (\x -> isTriangle x) i
  putStr "Part 2: "
  let i' = refactor i
  putStrLn . show $ length $ filter (\x -> isTriangle x) i'