josephus :: Integer -> Integer
josephus n = (2*l) + 1
  where p = last $ takeWhile (\x -> x <= n) [ 2^i | i <- [0,1..] ]
        l = n - p

josephus2 :: Integer -> Integer
josephus2 n
  | n == p3   = n
  | l <= p3   = l
  | otherwise = l + (n - 2 * p3)
  where p3 = last $ takeWhile (\x -> x <= n) [ 3^i | i <- [0,1..] ]
        l = n - p3

main :: IO()     
main = do
  putStr "Part 1: "
  putStrLn . show $ josephus 3014387
  putStr "Part 2: "
  putStrLn . show $ josephus2 3014387