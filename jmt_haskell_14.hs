import Numeric
import Data.List
import Data.Maybe
import Crypto.Hash
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

fst' (x,_,_) = x
snd' (_,x,_) = x
thd' (_,_,x) = x

hashString :: String -> String
hashString s = show (hashWith MD5 i)
  where i = C.pack s

repHash :: Int -> String -> String
repHash n s
  | n == 0    = s
  | otherwise = repHash (n-1) (hashString s)

hashInteger :: String -> Integer -> String
hashInteger s n = hashString (s ++ (show n))

repHashInteger :: String -> Integer -> String
repHashInteger s n = repHash 2017 (s ++ (show n))

firstTriple :: String -> Char
firstTriple s = head . (fromMaybe "*") $ find (\x -> length x >= 3) g 
  where g = group s

allPents :: String -> String
allPents s = map (head) $ filter (\x -> length x >= 5) g 
  where g = group s

findGood :: [(Integer, Char, String)] -> [Bool]
findGood [] = []
findGood (p:ps)
  | '*' == snd' p = [False] ++ (findGood ps)
  | otherwise     = [(or b)] ++ (findGood ps)
  where nextM = map (thd') $ take 1000 ps
        b = map (\x -> (snd' p) `elem` x) nextM

main :: IO()
main = do
  putStr "part 1: "
  let salt = "jlmsuwbz"
  let range = [0,1..]
  let hashes = map (hashInteger salt) range
  let t = map firstTriple hashes
  let q = map allPents hashes
  let htq = zip3 range t q
  let s = zip htq $ findGood htq
  let r = map (\((a,_,_),_) -> a) $ filter (snd) s
  putStrLn . show . last $ take 64 r 

  -- slow!
  putStr "part 2: "
  let hashes = map (repHashInteger salt) range
  let t = map firstTriple hashes
  let q = map allPents hashes
  let htq = zip3 range t q
  let s = zip htq $ findGood htq
  let r = map (\((a,_,_),_) -> a) $ filter (snd) s
  putStrLn . show . last $ take 64 r 
