import Data.List
import Data.List.Split

len :: [a] -> Integer
len x = toInteger $ length x

splitAtFirst :: Eq a => a -> [a] -> ([a],[a])
splitAtFirst x = fmap (drop 1) . break (x ==)

decomp :: String -> String
decomp s 
  | '(' `elem` s = a ++ (concat $ replicate n (take m d)) ++ (decomp $ drop m d)
  | otherwise = s
  where (a, b) = splitAtFirst '(' s
        (c, d) = splitAtFirst ')' b
        [m, n] = map (read) (splitOn "x" c) :: [Int]

dlen :: String -> Integer
dlen s
  | '(' `elem` s = (len a) + ((toInteger n) * dlen (take m d)) +  dlen (drop m d)
  | otherwise = (len s)
  where (a, b) = splitAtFirst '(' s
        (c, d) = splitAtFirst ')' b
        [m, n] = map (read) (splitOn "x" c) :: [Int]

main :: IO()
main = do
  f <- readFile "input_09.txt"
  let l = head $ lines f
  putStr "Part 1: "
  putStrLn . show . length $ decomp l

  putStr "Part 2: "
  putStrLn . show $ dlen l
