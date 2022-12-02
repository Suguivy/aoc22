import Data.Char

main :: IO ()
main = do
    contents <- readFile "input"
    putStrLn $ "Part 1: " ++ part1 contents
    putStrLn $ "Part 2: " ++ part2 contents

part1 :: String -> String
part1 = show . sum . map (getRoundPoints . words) . lines
    where getRoundPoints [a,x] = let ap = ord (head a) - 64
                                     bp = ord (head x) - 87
                                 in 3 * mod (bp - ap + 1) 3 + bp 
part2 :: String -> String
part2 = show . sum . map (getRoundPoints . words) . lines
    where getRoundPoints [a,x] = let ap = ord (head a) - 64
                                     bp = ord (head x) - 88
                                 in (((ap - 1) + (bp + 2)) `mod` 3) + 1 + bp * 3