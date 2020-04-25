import Data.List
import Data.Maybe

intTOBin :: Int -> [Int]
intTOBin 0 = []
intTOBin n = intTOBin ( n `quot` 2 ) ++ [ n `rem` 2 ]

binWrap :: Int -> Int -> String
binWrap p n = concat . reverse . map show $ intTOBin (n + p)

is01 :: String -> Bool
is01 "01" = True
is01 (x:y:xs) = b && (is01 xs)
  where b = (x == '0') && (y == '1')

-- The assembunny repeatedly prints out the reversed binary
-- representation of the input offset by the number 7x362 = 2534
-- e.g.         100 ---Offset--> 100 + 2534 == 2634
--             2634 ---Binary--> 101001001010
--     101001001010 --Reverse--> 010100100101
--     010100100101 ---Repeat--> 010100100101010100100101010100100101...
--
-- See decompiled_25.cpp for more details
--
-- We are looking for is a number that is mapped to 010101...

main :: IO()
main = do
  putStr "Part 1: "
  let t = find (\x -> is01 $ binWrap 2534 x) [0..1000]
  putStrLn . show $ fromJust t
