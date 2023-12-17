-- One

-- main = readFile "2.txt" >>= print . sum . map (proc . drop 1 . dropWhile (/= ' ')) . lines
--
-- proc xs = if (passLine $ drop 2 ys) then (read g) else 0
--   where (g, ys) = break (== ':') xs
--
-- passLine [] = True
-- passLine l = if (passGame xs) then passLine (drop 2 ys) else False
--   where (xs, ys) = break (== ';') l
--
-- passGame [] = True
-- passGame g = if (passColour xs) then passGame (drop 2 ys) else False
--   where (xs, ys) = break (== ',') g
--
-- passColour c = case (drop 1 y) of
--     "red" -> (read x) <= 12
--     "green" -> (read x) <= 13
--     "blue" -> (read x) <= 14
--     q -> error q
--   where (x, y) = break (== ' ') c

-- Two

main = readFile "2.txt" >>= print . sum . map (line 0 0 0 . drop 2 . dropWhile (/= ':')) . lines

line r g b [] = r * g * b
line r g b l = line r' g' b' (drop 2 ys)
  where
    (xs, ys) = break (== ';') l
    (r', g', b') = game r g b xs

game r g b [] = (r, g, b)
game r g b gm = game r' g' b' (drop 2 ys)
  where
    (xs, ys) = break (== ',') gm
    (r', g', b') = colour r g b xs

colour r g b c = case (drop 1 y) of
    "red" -> if x' > r then (x', g, b) else (r, g, b)
    "green" -> if x' > g then (r, x', b) else (r, g, b)
    "blue" -> if x' > b then (r, g, x') else (r, g, b)
  where
    (x, y) = break (== ' ') c
    x' = read x
