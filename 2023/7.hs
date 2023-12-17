import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M

main = readFile "7.txt" >>= print . map (mask . words) . lines

mask :: [String] -> (Map Char Int, Int)
mask (h:p:_) = (foldl' (\m -> \c -> M.insertWith (+) c 1 m) M.empty h, read p)