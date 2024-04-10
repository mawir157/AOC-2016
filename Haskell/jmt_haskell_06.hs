import Data.List

byChar ss = map (\x -> map (!!x) ss) [0..7]

mostComp x y = compare (snd y) (snd x)

leastComp x y = compare (snd x) (snd y)

getFreq comp ss = fst $ head ordered
  where freq = map (\x -> (head x, (length x))) $ (group. sort) ss 
        ordered = sortBy (comp) freq

main = do 
  f <- readFile "../input/input06.txt"
  let s = byChar $ lines f
  putStr "Show 1: "
  putStrLn . show $ map (getFreq mostComp) s
  putStr "Show 2: "
  putStrLn . show $ map (getFreq leastComp) s
