import Data.Set hiding (splitAt)

main :: IO ()
main = do
    contents <- readFile "input"
    putStrLn $ "Part 1: " ++ part1 contents
    putStrLn $ "Part 2: " ++ part2 contents

part1 :: String -> String
part1 = getMarker 4

part2 :: String -> String
part2 = getMarker 14

getMarker n xs = show . (+n) . length . takeWhile ((<n) . size . fromList) $ scanl (\c x -> tail c ++ [x]) first rest
    where (first, rest) = splitAt n xs