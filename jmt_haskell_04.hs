import Data.List.Split
import Data.List
import Data.Function (on)

freq :: (Ord a) => [a] -> [(a, Int)]
freq xs = map (\x -> (head x, length x)) . group . sort $ xs

type Room = ([String], Int, String)

parseInput :: String -> Room
parseInput s = (ss, n, check)
  where pt1 = splitOn "-" $ takeWhile (\x -> x /= '[') s
        ss = init pt1
        n = read (last pt1) :: Int
        check = takeWhile (\x -> x /= ']') . drop 1 $ dropWhile (\x -> x /= '[') s

checkSum :: [String] -> String
checkSum ss = out
  where tt = sort . freq $ concat ss
        m = sortBy (flip compare `on` snd) tt
        out = map fst $ take 5 m

goodRoom :: Room -> Bool
goodRoom (ss, n, check) = (checkSum ss) == check

decode :: Room -> String
decode (ss, n, check) = u
  where u = map (\s -> shiftLetter n s) . unwords $ ss

shiftLetter :: Int -> Char -> Char
shiftLetter n c
  | c == ' '  = ' '
  | otherwise = toEnum (97 + (((fromEnum c) - 97 + n) `mod` 26))

fst' (x,_,_) = x
snd' (_,x,_) = x
thd' (_,_,x) = x

main = do 
  f <- readFile "input_04.txt"
  let i = map parseInput $ lines f
  let gr = filter (\x -> goodRoom x) i
  putStr "Part 1: "
  putStrLn . show . sum $ map snd' gr
  putStr "Part 2: "
  let d = map (\x -> (decode x, snd' x)) gr
  putStrLn . show $ filter (\x -> isInfixOf "north" (fst x)) d
