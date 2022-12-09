import Data.List.Split
import Data.Bifunctor
import qualified Data.Set as S

main :: IO ()
main = do
    contents <- readFile "input"
    putStrLn $ "Part 1: " ++ part1 contents
    putStrLn $ "Part 2: " ++ part2 contents

part1 :: String -> String
part1 = show . S.size . moveRope (replicate 2 (0,0)) . parseInput

part2 :: String -> String
part2 = show . S.size . moveRope (replicate 10 (0,0)) . parseInput

moveRope :: [(Int, Int)] -> [(Int, Int)] -> S.Set (Int, Int)
moveRope = moveRope' S.empty
    where moveRope' visited rope [] = visited
          moveRope' visited rope (m:moves) = moveRope' (S.insert (last rope) visited) (moveKnots rope m) moves

moveKnots :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
moveKnots ((hx,hy):t) (x,y) = foldl (\ac x -> ac ++ [moveKnot (last ac) x]) [(hx+x,hy+y)] t
    where moveKnot (hx,hy) (tx,ty) = let dx = hx - tx; dy = hy - ty in case (dx, dy) of
            (2,2) -> (tx+1, ty+1)
            (-2,2) -> (tx-1, ty+1)
            (2,-2) -> (tx+1, ty-1)
            (-2,-2) -> (tx-1, ty-1)
            (2,y) -> (tx+1,ty+y)
            (-2,y) -> (tx-1,ty+y)
            (x,2) -> (tx+x,ty+1)
            (x,-2) -> (tx+x,ty-1)
            _ -> (tx,ty)

parseInput :: String -> [(Int, Int)]
parseInput = concatMap (toMove . bimap head read . splitAt 2) . lines
    where toMove ('U', n) = replicate n (0,1)
          toMove ('D', n) = replicate n (0,-1)
          toMove ('L', n) = replicate n (-1,0)
          toMove ('R', n) = replicate n (1,0)