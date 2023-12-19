import Data.List (foldl')
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

-- One

-- main = readFile "8.txt" >>= print . proc . lines
--
-- proc (x:_:xs) = move x m (m ! "AAA") x where
--   m = (foldl' parse) M.empty xs
--
-- parse m s = M.insert n (l,r) m where
--   (n, ns) = splitAt 3 s
--   (l, ls) = splitAt 3 $ drop 4 ns
--   r = take 3 $ drop 2 ls
--
-- move dds m x [] = move dds m x dds
-- move dds m (x,_) ('L':ds) = if x == "ZZZ" then 1 else 1 + move dds m (m ! x) ds
-- move dds m (_,x) ('R':ds) = if x == "ZZZ" then 1 else 1 + move dds m (m ! x) ds





-- Two

main = readFile "8.txt" >>= print . proc . lines

proc (ds:_:xs) = foldl' (\m -> \x -> lcm x m) a as where
  m = (foldl' parse) M.empty xs
  (a:as) = map (\x -> move ds m (m ! x) ds) $ filter (\x -> (last x) == 'A') $ M.keys m

parse m s = M.insert n (l,r) m where
  (n, ns) = splitAt 3 s
  (l, ls) = splitAt 3 $ drop 4 ns
  r = take 3 $ drop 2 ls

move dds m x [] = move dds m x dds
move dds m (x,_) ('L':ds) = if (last x) == 'Z' then 1 else 1 + move dds m (m ! x) ds
move dds m (_,x) ('R':ds) = if (last x) == 'Z' then 1 else 1 + move dds m (m ! x) ds
