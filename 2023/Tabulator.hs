module Tabulator
  ( tabulate
  , printLines
  , writeLines
  ) where

import Data.List (transpose, foldl')
import Data.Char (isMark)

tabulate :: [[String]] -> [String]
tabulate ts = map (concatMap (\(l,x) -> pad l x) . zip (map maxLength $ transpose ts)) ts

tabulateRow :: [String] -> String
tabulateRow ts = concatMap (pad (maxLength ts)) ts

altTabulate :: [[String]] -> [String]
altTabulate ts = map (concatMap (\((l,t),x) -> (if t then padL else pad) l x) . zip lens) ts
  where
    lens = zip (map maxLength $ transpose ts) (iterate not True)

altTabulateWith :: String -> [[String]] -> [String]
altTabulateWith sep ts = map (concat . intersperse2 sep . map (\((l,t),x) -> (if t then padL else pad) l x) . zip lens) ts
  where
    lens = zip (map maxLength $ transpose ts) (iterate not True)

maxLength :: [String] -> Int
maxLength = (+) 1 . foldl' (\max str -> let l = displayLength str in if (l > max) then l else max) 0

pad :: Int -> String -> String
pad i _ | i <= 0 = []
pad i [] = replicate i ' '
pad i (x:xs) = x : pad (if (not $ isMark x) then (i-1) else i) xs

padL :: Int -> String -> String
padL i = reverse . pad i . reverse

displayLength :: String -> Int
displayLength = length . filter (not . isMark)

intersperse2 :: a -> [a] -> [a]
intersperse2 _ [] = []
intersperse2 sep (x:xs) = x : (fst $ foldl' skip ([], False) xs)
  where
    skip (ys, False) y = let ys' = y : ys in seq ys' (ys', True)
    skip (ys, True) y = let ys' = sep : y : ys in seq ys' (ys', False)

printLines :: [String] -> IO ()
printLines [] = pure ()
printLines (x:xs) = putStrLn x >> printLines xs

writeLines :: FilePath -> [String] -> IO ()
writeLines _ [] = pure ()
writeLines p (x:xs) = appendFile p x >> writeLines p xs
