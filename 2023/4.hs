import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

-- One

-- main = readFile "4.txt" >>= print . foldl' proc 0 . lines
--
-- proc s l = s + (wins $ drop 2 $ dropWhile (/= ':') l)
--
-- wins xs = let s = (Set.size $ Set.intersection ls' rs') in if s == 0 then 0 else 2 ^ (s-1)
--   where
--     (ls, rs) = break (== '|') xs
--     ts :: String -> Set Int
--     ts = Set.fromList . map read . words
--     ls' = ts ls
--     rs' = ts $ drop 2 rs

-- Two

main = readFile "4.txt" >>= print . proc . lines

proc = foldl' (\s -> \(n,_) -> s + n) 0 . card . map (\l -> (1, wins l))

card [] = []
card (x@(n,c):xs) = x : (card $ addc c n xs)

addc 0 _ xs = xs
addc i c ((n,w):xs) = (n+c,w) : addc (i-1) c xs

wins xs = Set.size $ Set.intersection ls' rs'
  where
    (ls, rs) = break (== '|') $ drop 2 $ dropWhile (/= ':') xs
    ts :: String -> Set Int
    ts = Set.fromList . map read . words
    ls' = ts ls
    rs' = ts $ drop 2 rs