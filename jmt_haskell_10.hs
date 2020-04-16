import Data.List
import Data.List.Split

type Bot =  (Integer, [Integer])
type MoveRule = (Integer, (Integer, Integer))

removeFromBot :: [Bot] -> Integer -> Integer -> [Bot]
removeFromBot bs i v
  | length v'' > 0 = bs' ++ [(i, v'')]
  | otherwise      = bs'
  where bs' = filter (\x -> fst x /= i) bs
        (_,v') = head $ filter (\x -> fst x == i) bs
        v'' = filter (\x -> x /= v) v'

addToBot :: [Bot] -> Integer -> Integer -> [Bot]
addToBot bs i v
  | i `elem` seenIds = bs' ++ [(i, sort ([v] ++ v'))]
  | otherwise        = bs  ++ [(i, [v])]
  where seenIds = map (fst) bs
        bs' = filter (\x -> fst x /= i) bs
        (_,v') = head $ filter (\x -> fst x == i) bs

buildRules :: String -> MoveRule
buildRules s = (ii, (lo, hi))
  where ss = (splitOn " " s) 
        ii = read (ss!!1) :: Integer
        lo = read (ss!!6) :: Integer
        hi = read (ss!!11) :: Integer

initialBots :: [Bot] -> String -> [Bot]
initialBots bs s = addToBot bs i v
  where sp = (splitOn " " s) 
        i  = read (sp!!5) :: Integer
        v  = read (sp!!1) :: Integer

click :: [Bot] -> [MoveRule] -> [Bot]
click bs rs = reverse bsh -- i have no idea why reverse is needed here!
  where bad = filter (\(_,y) -> length y > 1) bs
        (i, [lo', hi']) = head $ bad
        (_, (loId, hiId)) = head $ filter (\(x,_) -> x == i) rs
        bsl = addToBot (removeFromBot bs i lo') loId lo'
        bsh = addToBot (removeFromBot bsl i hi') hiId hi'

click' :: [Bot] -> [MoveRule] -> [Bot]
click' bs rs
  | length bad > 0 = click' (click bs rs) rs
  | otherwise      = bs
  where bad = filter (\(_,y) -> length y > 1) bs

findCompare :: [Integer] -> [Bot] -> [MoveRule] -> [Bot]
findCompare v bs rs
  | length cmp == 1 = bs
  | length bad > 0 = findCompare v (click bs rs) rs
  | otherwise      = bs
  where bad = filter (\(_,y) -> length y > 1) bs
        cmp = filter (\(_,y) -> y == v) bs

main :: IO()
main = do
  f <- readFile "input_10.txt"
  let l = lines f
  let bots = foldl initialBots [] (filter (\x -> (head x) == 'v') l)
  let rules = map (buildRules) $ filter (\x -> (head x) == 'b') l

  putStr "Part 1: "
  let p = [17,61]
  let fc = findCompare p bots rules
  putStrLn . show . fst . head $ filter (\(_, y) ->  y == p) fc

  putStr "Part 2: "
  let p2 = sort $ click' bots rules
  putStrLn . show . foldl1 (*).  concat . map (snd) $ take 3 p2
