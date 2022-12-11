import Data.List.Split

main :: IO ()
main = do
    contents <- readFile "input"
    putStrLn $ "Part 1: " ++ part1 contents
    putStrLn $ "Part 2: " ++ part2 contents

part1 :: String -> String
part1 input = show . sum . map (\cycle -> (*cycle) . fst $ (parseStates input !! (cycle-1))) $ [20,60..220]


part2 :: String -> String
part2 = ('\n':) . drawSprite 

drawSprite :: String -> String
drawSprite = drawInLines . map drawPixel . parseStates
    where drawPixel (x, cycle) = if cycle `mod` 40 >= x-1 && cycle `mod` 40 <= x+1 then '#' else '.'

parseStates :: String -> [(Int, Int)]
parseStates = foldl (\ac x -> ac ++ parseIns (last ac) x)  [(1,0)] . map (splitOn " ") . lines
    where parseIns (x, cycles) ["addx", n] = [(x, cycles + 1), (x + read n, cycles + 2)]
          parseIns (x, cycles) ["noop"] = [(x, cycles + 1)]

drawInLines :: String -> String
drawInLines [] = []
drawInLines l = take 40 l ++ ('\n':drawInLines (drop 40 l))