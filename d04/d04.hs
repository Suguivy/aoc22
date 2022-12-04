import Data.Bifunctor

main :: IO ()
main = do
    contents <- readFile "input"
    putStrLn $ "Part 1: " ++ part1 contents
    putStrLn $ "Part 2: " ++ part2 contents

part1 :: String -> String
part1 = show . length . filter id . map (uncurry isSubrange . bimap parseRange (parseRange . drop 1) . span (/= ',')) . lines
    where parseRange :: String -> (Int, Int)
          parseRange = bimap read (read . drop 1) . span (/= '-')
          isSubrange (a,b) (c,d) = not $ (a > c && b > d) || (a < c && b < d)

part2 :: String -> String
part2 = show . length . filter id . map (uncurry areOverlapping . bimap parseRange (parseRange . drop 1) . span (/= ',')) . lines
    where parseRange :: String -> (Int, Int)
          parseRange = bimap read (read . drop 1) . span (/= '-')
          areOverlapping (a,b) (c,d) = not $ (a < c && b < c) || (a > d && b > d)
