import Data.Maybe (catMaybes)
import Data.Char (isDigit)
import Prelude hiding (scanl, scanr)

-- One

-- main = readFile "3.txt" >>= print . proc (repeat '.') . lines
--
-- proc :: String -> [String] -> Int
-- proc prv (cur:[]) = dot False prv cur (repeat '.')
-- proc prv (cur:xs@(nxt:_)) = (dot False prv cur nxt) + proc cur xs
--
-- dot _ _ [] _ = 0
-- dot p (prv:px) (cur:cx) (nxt:nx) = if (isDigit cur)
--   then numeric [cur] (p || prv /= '.' || nxt /= '.') px cx nx
--   else dot (cur /= '.' || prv /= '.' || nxt /= '.') px cx nx
--
-- numeric n p _ [] _ = if p then (read $ reverse n) else 0
-- numeric n p (prv:px) (cur:cx) (nxt:nx) = if (isDigit cur)
--   then numeric (cur : n) (p || prv /= '.' || nxt /= '.') px cx nx
--   else (if (p || cur /= '.' || prv /= '.' || nxt /= '.') then (read $ reverse n) else 0) + dot False (prv:px) (cur:cx) (nxt:nx)

-- Two

main = readFile "3.txt" >>= print . proc2 . proc (tozip $ repeat '.') . map tozip . lines

proc2 = foldl (\s -> \(x,y) -> s + ((read x) * (read y))) 0

proc prv (cur:[]) = notstar [] prv cur (tozip $ repeat '.')
proc prv (cur:xs@(nxt:_)) = (notstar [] prv cur nxt) ++ proc cur xs

tozip (x:xs) = ([], x, xs)
at (_, x, _) = x
sftl ([], x, rs) = ([], x, rs)
sftl ((l:ls), x, rs) = (ls, l, x : rs)
sftr (ls, x, []) = (ls, x, [])
sftr (ls, x, (r:rs)) = (x : ls, r, rs)

scanl = scanl' [] where
  scanl' n ([], x, _) = if (isDigit x) then (x : n) else n
  scanl' n xs = if (isDigit $ at xs) then scanl' (at xs : n) (sftl xs) else n

scanr = scanr' [] where
  scanr' n (_, x, []) = reverse $ if (isDigit x) then (x : n) else n
  scanr' n xs = if (isDigit $ at xs) then scanr' (at xs : n) (sftr xs) else reverse n

scanb xs = (scanl xs) ++ (scanr (sftr xs))

notstar xs _ (_, _, []) _ = catMaybes xs
notstar xs px cx nx = notstar xs' (sftr px) (sftr cx) (sftr nx)
  where xs' = if (at cx) == '*' then (star px cx nx) : xs else xs

star px cx nx = if (length xs == 2) then Just (head xs, last xs) else Nothing
  where
    xs = catMaybes $
      [ tryscan scanl (sftl cx) -- Left
      , tryscan scanr (sftr cx) -- Right
      ] ++ topbot px -- Top
        ++ topbot nx -- Bottom

tryscan f x = let r = f x in if r == [] then Nothing else Just r

topbot xs = if (isDigit $ at xs)
  then [ tryscan scanb xs ]
  else [ tryscan scanl (sftl xs), tryscan scanr (sftr xs) ]