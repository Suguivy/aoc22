import Data.List.Split

main :: IO ()
main = do
    contents <- readFile "input"
    putStrLn $ "Part 1: " ++ part1 contents
    putStrLn $ "Part 2: " ++ part2 contents

part1 :: String -> String
part1 input = let [crates, moves] = splitOn "\n\n" input in peekCrates $ moveCrates (parseCrates crates) (parseMoves moves) True

part2 :: String -> String
part2 input = let [crates, moves] = splitOn "\n\n" input in peekCrates $ moveCrates (parseCrates crates) (parseMoves moves) False

peekCrates :: [[Char]] -> String
peekCrates = map head

moveCrates :: [[Char]] -> [[Int]] -> Bool -> [[Char]]
moveCrates crates []                    oneAtATime = crates
moveCrates crates ([n, from, to]:moves) oneAtATime = moveCrates (move crates n (from-1) (to-1)) moves oneAtATime
    where set i v l = let (before, after) = splitAt i l in before ++ (v : drop 1 after)
          move crates' n from to = let (picked, remaining) = splitAt n $ crates' !! from
                                   in set to ((if oneAtATime then reverse else id) picked ++ (crates' !! to)) . set from remaining $ crates'

parseMoves :: String -> [[Int]]
parseMoves = map (map (read . snd) . filter (odd . fst) . zip [0..] . words) . lines

parseCrates :: String -> [[Char]]
parseCrates crates = map (dropWhile (==' ') . reverse)
            . foldr (zipWith (:) . parseCratesRow) (replicate numOfStacks [])
            . drop 1
            . reverse
            . lines
            $ crates
    where parseCratesRow row = map ((row !!) . succ . (*4)) [0..numOfStacks-1]
       -- parseCratesRow: "[S]     [T]" -> "S T"
          numOfStacks = (`div` 4) . succ . length . head . lines $ crates
