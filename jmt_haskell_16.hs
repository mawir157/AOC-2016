if' True  x _ = x
if' False _ x = x

reduce :: String -> String
reduce bs
  | odd $ length bs = bs
  | otherwise       = reduce $ f bs
  where f [] = []
        f (x:y:xs) = [(if' (x==y) '1' '0')] ++ (f xs)

dragon :: Int -> String -> String
dragon n bs
  | length bs < n = dragon n bs'
  | otherwise     = take n bs
  where bs' = bs ++ "0" 
                 ++ (reverse $! map (\x -> if' (x=='0') '1' '0') bs)

main :: IO()     
main = do 
  let seed = "11101000110010100"
  let done =  reduce $ dragon 272 seed
  putStr "Part 1: "
  putStrLn done

  putStr "Part 2: "
  let p = dragon (35651584) seed
  putStrLn  $ reduce p
