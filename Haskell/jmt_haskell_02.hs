finger :: Char -> Char -> Char
finger x c = 
  if x == '1' then
    if c == 'U' then '1'
    else if c == 'L' then '1'
    else if c == 'D' then '4'
    else if c == 'R' then '2'
    else '0'
  else if x == '2' then
    if c == 'U' then '2'
    else if c == 'L' then '1'
    else if c == 'D' then '5'
    else if c == 'R' then '3'
    else '0'
  else if x == '3' then
    if c == 'U' then '3'
    else if c == 'L' then '2'
    else if c == 'D' then '6'
    else if c == 'R' then '3'
    else '0'
  else if x == '4' then
    if c == 'U' then '1'
    else if c == 'L' then '4'
    else if c == 'D' then '7'
    else if c == 'R' then '5'
    else '0'
  else if x == '5' then
    if c == 'U' then '2'
    else if c == 'L' then '4'
    else if c == 'D' then '8'
    else if c == 'R' then '6'
    else '0'
  else if x == '6' then
    if c == 'U' then '3'
    else if c == 'L' then '5'
    else if c == 'D' then '9'
    else if c == 'R' then '6'
    else '0'
  else if x == '7' then
    if c == 'U' then '4'
    else if c == 'L' then '7'
    else if c == 'D' then '7'
    else if c == 'R' then '8'
    else '0'
  else if x == '8' then
    if c == 'U' then '5'
    else if c == 'L' then '7'
    else if c == 'D' then '8'
    else if c == 'R' then '9'
    else '0'
  else if x == '9' then
    if c == 'U' then '6'
    else if c == 'L' then '8'
    else if c == 'D' then '9'
    else if c == 'R' then '9'
    else '0'
  else 
    '0'

finger2 :: Char -> Char -> Char
finger2 x c =
  if x == '1' then
    if c == 'D' then '3'
    else '1'
  else if x == '2' then
    if c == 'D' then '6'
    else if c == 'R' then '3'
    else '2'
  else if x == '3' then
    if c == 'U' then '1'
    else if c == 'L' then '2'
    else if c == 'D' then '7'
    else if c == 'R' then '4'
    else '!'
  else if x == '4' then
    if c == 'L' then '3'
    else if c == 'D' then '8'
    else '4'
  else if x == '5' then
    if c == 'R' then '6'
    else  '5'
  else if x == '6' then
    if c == 'U' then '2'
    else if c == 'L' then '5'
    else if c == 'D' then 'A'
    else if c == 'R' then '7'
    else '£'
  else if x == '7' then
    if c == 'U' then '3'
    else if c == 'L' then '6'
    else if c == 'D' then 'B'
    else if c == 'R' then '8'
    else '%'
  else if x == '8' then
    if c == 'U' then '4'
    else if c == 'L' then '7'
    else if c == 'D' then 'C'
    else if c == 'R' then '9'
    else '^'
  else if x == '9' then
    if c == 'L' then '8'
    else '9'
  else if x == 'A' then
    if c == 'U' then '6'
    else if c == 'R' then 'B'
    else 'A'
  else if x == 'B' then
    if c == 'U' then '7'
    else if c == 'L' then 'A'
    else if c == 'D' then 'D'
    else if c == 'R' then 'C'
    else '&'
  else if x == 'C' then
    if c == 'U' then '8'
    else if c == 'L' then 'B'
    else 'C'
  else if x == 'D' then
    if c == 'U' then 'B'
    else 'D'
  else 
    '*'

getPress :: Char -> String -> Char
getPress n ss = foldl finger n ss

getPress2 :: Char -> String -> Char
getPress2 n ss = foldl finger2 n ss

main = do 
  f <- readFile "../input/input02.txt"
  let i = lines f
  putStr "Part 1: "
  putStrLn . show $ drop 1 $ scanl getPress '5' i
  putStr "Part 2: "
  putStrLn . show $ drop 1 $ scanl getPress2 '5' i
