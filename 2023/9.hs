import Data.List (foldl')

-- One

-- main = readFile "9.txt" >>= print . foldl' proc 0 . lines
--
-- proc s x = s + ((next . reverse . map read . words) x)
--
-- next x = if all (== 0) x'
--     then head x
--     else (head x) + next x'
--   where
--     x' = delta x
--
-- delta (x:xs@(y:_)) = (x - y) : delta xs
-- delta _ = []





-- Two

main = readFile "9.txt" >>= print . foldl' proc 0 . lines

proc s x = s + ((next . map read . words) x)

next x = if all (== 0) x'
    then head x
    else (head x) + next x'
  where
    x' = delta x

delta (x:xs@(y:_)) = (x - y) : delta xs
delta _ = []