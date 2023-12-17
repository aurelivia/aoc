import Data.Char

main = readFile "1.txt" >>= print . sum . map (read . (\x -> (head x) : [last x]) . nums) . lines

nums :: String -> String
nums [] = []
nums (x:xs@(y:ys)) = case (x,y) of
    -- ('z', 'e') -> match '0' "ro" xs ys
    ('o', 'n') -> match '1' "e" 'e' xs ys
    ('t', 'w') -> match '2' "o" 'o' xs ys
    ('t', 'h') -> match '3' "ree" 'e'  xs ys
    ('f', 'o') -> match '4' "ur" 'r' xs ys
    ('f', 'i') -> match '5' "ve" 'e' xs ys
    ('s', 'i') -> match '6' "x" 'x' xs ys
    ('s', 'e') -> match '7' "ven" 'n' xs ys
    ('e', 'i') -> match '8' "ght" 't' xs ys
    ('n', 'i') -> match '9' "ne" 'e' xs ys
    _ -> if isDigit x then x : nums xs else nums xs
  where
    match :: Char -> String -> Char -> String -> String -> String
    match i [] _ _ [] = [i]
    match _ _ _ _ [] = []
    match i [] c _ ys = i : nums (c : ys)
    match i (x:xs) c ys' (y:ys) = if x == y then match i xs c ys' ys else nums ys'
nums (x:xs) = if isDigit x then x : nums xs else nums xs