import Numeric
import Crypto.Hash
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

import Debug.Trace
import Data.List
import Data.Maybe

if' True  x _ = x
if' False _ x = x

type Pos = (Integer, Integer)

routeHash  :: String -> String
routeHash s = take 4 $ show (hashWith MD5 (C.pack s))

hashDirs :: String -> [Bool]
hashDirs s = m'
  where m = routeHash s
        m' = map (\x -> x `elem` "bcdef") m

neighbours :: (Pos,String) -> [(Pos,String)]
neighbours ((x,y), route)  = du ++ dd ++ dl ++ dr
  where [bu,bd,bl,br] = hashDirs route
        du = if' (bu && y /= 0) [((x, y-1), route ++ "U")] []
        dd = if' (bd && y /= 3) [((x, y+1), route ++ "D")] []
        dl = if' (bl && x /= 0) [((x-1, y), route ++ "L")] []
        dr = if' (br && x /= 3) [((x+1, y), route ++ "R")] []

end :: (Pos, String) -> Bool
end (p, _) = (p == (3,3))

bfSearch :: [(Pos, String)] -> String
bfSearch x
  | any (end) x      = snd . fromJust $ find (end) x
  | length nbrs == 0 = "FAILED TO FIND" 
  | otherwise        = bfSearch nbrs
  where nbrs = concat $ map (neighbours) x

maxSearch :: (Pos, String) -> Int
maxSearch (p, s)
  | end (p,s)        = length s
  | length nbrs == 0 = 0
  | otherwise        = maximum $ map (maxSearch) nbrs
  where nbrs = (neighbours) (p, s)

main :: IO()
main = do
  let passcode = "rrrbmfta"
  putStr "Part 1: "
  let s = bfSearch [((0,0), passcode)]
  putStrLn $ show s

  let passcode = "rrrbmfta"
  putStr "Part 2: "
  let s = maxSearch ((0,0), passcode)
  putStrLn $ show (s - length passcode)
