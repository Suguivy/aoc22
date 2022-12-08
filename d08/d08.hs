import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Char
import Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "input"
    putStrLn $ "Part 1: " ++ part1 contents
    putStrLn $ "Part 2: " ++ part2 contents

part1 :: String -> String
part1 input = show . length . filter id . map (isVisible m) $ [(r, c) | r <- [1..M.nrows m], c <- [1..M.ncols m]]
    where m = M.fromLists . map (map digitToInt) $ lines input

part2 :: String -> String
part2 input = show . maximum . map (getScore m) $ [(r, c) | r <- [1..M.nrows m], c <- [1..M.ncols m]]
    where m = M.fromLists . map (map digitToInt) $ lines input

isVisible :: M.Matrix Int -> (Int, Int) -> Bool
isVisible m (r, c) = any (null . V.filter (>=tree)) [columnAbove, columnBelow, rowAbove, rowBelow]
    where tree = m M.! (r, c)
          (columnAbove, columnBelow) = second V.tail $ V.splitAt (r-1) (M.getCol c m)
          (rowAbove,    rowBelow)    = second V.tail $ V.splitAt (c-1) (M.getRow r m)

getScore :: M.Matrix Int -> (Int, Int) -> Int
getScore m (r, c) = product . map ((\t -> V.length (fst t) + if null $ snd t then 0 else 1) . V.span (<tree)) $ [columnAbove, columnBelow, rowAbove, rowBelow]
    where tree = m M.! (r, c)
          (columnAbove, columnBelow) = bimap V.reverse V.tail $ V.splitAt (r-1) (M.getCol c m)
          (rowAbove,    rowBelow)    = bimap V.reverse V.tail $ V.splitAt (c-1) (M.getRow r m)