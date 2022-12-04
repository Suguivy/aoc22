import Data.Set hiding (map, splitAt, take)

main :: IO ()
main = do
    contents <- readFile "input"
    putStrLn $ "Part 1: " ++ part1 contents
    putStrLn $ "Part 2: " ++ part2 contents

part1 :: String -> String
part1 = show . sum . map (getPriority . elemAt 0 . (\t -> fromList (fst t) `intersection` fromList (snd t)) . divide) . lines 
    where divide s = splitAt (length s `div` 2) s

part2 :: String -> String
part2 = show . sum . map (getPriority . elemAt 0 . (\t -> let (x,y,z) = t in foldr1 intersection (map fromList [x,y,z]))) . grups . lines
    where grups [] = []
          grups (x:y:z:xs) = (x,y,z):grups xs

getPriority :: Char -> Int
getPriority c = succ . length . takeWhile (/= c) $ ['a'..'z'] ++ ['A'..'Z'] 