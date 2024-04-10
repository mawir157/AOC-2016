resClass (p,r) o xs = [ x | x <- xs, ((x + (r + o)) `mod` p) == 0]

findGap [] _ xs = xs
findGap (d:ds) n xs = findGap ds (n+1) (resClass d n xs)

main :: IO()     
main = do 
  let discs = [(13,11),(5,0),(17,11),(3,0),(7,2),(19,17)]
  
  putStr "Part 1: "
  let k = findGap discs 1 [0,1..]
  putStrLn . show . head $ k

  putStr "Part 2: "
  let k' = findGap (discs ++ [(11, 0)]) 1 k
  putStrLn . show . head $ k'
