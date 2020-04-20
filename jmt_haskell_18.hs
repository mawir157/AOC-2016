import Data.List

if' True  x _ = x
if' False _ x = x

bad = ["^^.", ".^^", "^..", "..^"]

blocks ::  Int -> [a] -> [[a]]
blocks n xs
  | length xs == n = [xs]
  | otherwise      = [take n xs] ++ blocks n (drop 1 xs)

trap :: String -> Char
trap s = if' (s `elem` bad) '^' '.'

nextRow :: String -> String
nextRow s = map (trap) $ blocks 3 ("." ++ s ++ ".")

countRow :: String -> Integer
countRow [] = 0
countRow (s:ss) = (if' (s == '.') 1 0) + countRow ss

strictCount :: Integer -> (String, Integer) -> (String, Integer)
strictCount 0 (s, i) = (s, i)
strictCount n (s, i) = strictCount (n-1) (s',i')
  where !s' = nextRow s
        !i' = i + countRow s

main :: IO()     
main = do
  f <- readFile "input_18.txt"
  let s = head $ lines f
  putStr "Part 1: "
  let s' = take 40 $ iterate nextRow s
  putStrLn . show . sum $ map (countRow) s'

  putStr "Part 2: "
  putStrLn . show . snd $ strictCount 400000 (s, 0)
