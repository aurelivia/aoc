
-- One

-- main = readFile "6.txt" >>= print . product . map length . proc . (\(x:y:[]) -> zip x y) . map (drop 1 . words) . lines
--
-- proc [] = []
-- proc ((t,d):xs) = (filter (\(x,y) -> y > d') $ times t') : proc xs where
--   d' = read d; t' = read t
--
-- times t = times' t (t-1) where
--   times' t 0 = []
--   times' t h = (h, ((t - h) * h)) : times' t (h-1)

-- Two

main = readFile "6.txt" >>= print . length . proc . map (concat . drop 1 . words) . lines

proc (t:d:_) = (filter (\(x,y) -> y > d') $ times t') where
  d' = read d; t' = read t

times t = times' t (t-1) where
  times' t 0 = []
  times' t h = (h, ((t - h) * h)) : times' t (h-1)