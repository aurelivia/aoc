import Data.List (foldl')
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as M

-- One

main = readFile "5.txt" >>= print . group . drop 2 . dropWhile (/= ':')

group :: String -> [[Int]]
group = map (map read') . reverse . group' [] [] . reverse where
  group' :: String -> [String] -> String -> [[String]]
  group' g gs [] = case gs of
    [] -> case g of { [] -> []; _ -> [[g]] }
    _ -> case g of { [] -> [gs]; _ -> [g:gs] }
  group' g gs (' ':xs) = group' [] (g:gs) xs
  group' g gs ('\n':xs) = case gs of
    [] -> case g of { [] -> group' [] [] xs; _ -> [g] : group' [] [] xs }
    _ -> case g of { [] -> gs : group' [] [] xs; _ -> (g:gs) : group' [] [] xs }
  group' g gs (x:xs) = group' g' gs xs where
    g' = if (isDigit x) then (x : g) else g
  read' "" = -1
  read' x = read x

proc xs = seed2soil where
  (sds, sds') = break (== [-1]) xs
  (s2s, s2s') = break (== [-1]) $ drop 1 sds'
  (s2f, s2f') = break (== [-1]) $ drop 1 s2s'
  (f2w, f2w') = break (== [-1]) $ drop 1 s2f'
  (w2l, w2l') = break (== [-1]) $ drop 1 f2w'
  (l2t, l2t') = break (== [-1]) $ drop 1 w2l'
  (t2h, t2h') = break (== [-1]) $ drop 1 l2t'
  (h2l, _)    = break (== [-1]) $ drop 1 t2h'