import Numeric
import Crypto.Hash
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

findSingle :: String -> Integer -> (Integer, Char)
findSingle p n
  | (take 5 h) == "00000" = (n, h!!5)
  | otherwise           = findSingle p (n+1)
  where i = C.pack $ p ++ (show n)
        h = show (hashWith MD5 i)

findPair :: String -> Integer -> (Integer, (Char, Char))
findPair p n
  | (take 5 h) == "00000" = (n, (h!!5, h!!6))
  | otherwise             = findPair p (n+1)
  where i = C.pack $ p ++ (show n)
        h = show (hashWith MD5 i)

getCode :: String -> Integer -> String
getCode p n = [c] ++ getCode p (n' + 1)
  where (n', c) = findSingle p n

insertChar :: String -> Int -> Char -> String
insertChar s i c
  | i >= 8      = s
  | s!!i /= '*' = s
  | otherwise   = (take i s) ++ [c] ++ (drop (i + 1) s)

getCode2 :: String -> Integer -> String -> String
getCode2 p n code
  | '*' `elem` code' = getCode2 p (n' + 1) code'
  | otherwise        = code'
  where (n', (a,b)) = findPair p n
        a' = fst $ head (readHex [a]) :: Int
        code' = insertChar code a' b

main :: IO()
main = do
  let i = "ffykfhsq"
  putStr "part 1: "
  putStrLn . take 8 $ getCode i 0
  putStr "part 2: "
  let k = getCode2 i 0 "********"
  putStrLn k

