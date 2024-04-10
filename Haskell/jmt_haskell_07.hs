import Data.List

letters = "abcdefghijklmnopqrstuvwxyz"

goodStrings :: [String]
goodStrings = [[x]++[y]++[y]++[x] | x <- letters, y <- letters, x/=y]

part2Pairs :: [(String,String)]
part2Pairs = [ ([x]++[y]++[x],[y]++[x]++[y]) | x <- letters, y <- letters, x/=y]

parseInput :: ([String],[String]) -> String -> ([String],[String])
parseInput (as,bs) "" = (as,bs)
parseInput (as,bs) ss
  | head ss == '[' = parseInput (as, bs ++ [b]) bs'
  | otherwise      = parseInput (as ++ [a], bs) as'
  where b = takeWhile (\x -> x/=']') $ drop 1 ss
        bs' = drop 1 $ dropWhile (\x -> x/=']') ss
        a = takeWhile (\x -> x/='[') ss
        as' = dropWhile (\x -> x/='[') ss

hasGoodString :: String -> Bool
hasGoodString s = or $ map (\x -> isInfixOf x s) goodStrings

goodCode :: ([String],[String]) -> Bool
goodCode (xs, ys) = xb && (not yb)
  where xb = or $ map (hasGoodString) xs
        yb = or $ map (hasGoodString) ys

matchPair :: ([String],[String]) -> (String, String) -> Bool
matchPair (outs, ins) (lhs,rhs) = a && b
  where a = or $ map (\x -> isInfixOf lhs x) outs 
        b = or $ map (\x -> isInfixOf rhs x) ins

part2Match :: ([String],[String]) -> Bool
part2Match codes = or $ map (matchPair codes) part2Pairs

main = do 
  f <- readFile "../input/input07.txt"
  let i = map (parseInput ([],[])) $ lines f
  let k = map goodCode i
  putStr "Part 1: "
  putStrLn . show . length $ filter (\x -> x) k
  let s = map (part2Match) i
  putStr "Part 2: "
  putStrLn . show . length $ filter (\x -> x) s
