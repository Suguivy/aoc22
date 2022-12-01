main :: IO ()
main = do
    contents <- readFile "input"
    putStrLn $ "Part 1: " ++ part1 contents
    putStrLn $ "Part 2: " ++ part2 contents

part1 :: String -> String
part1 = show . maximum . map (sum . map read . lines) . splitWithStr "\n\n"

part2 :: String -> String
part2 l = show $ a + b + c
    where (a,b,c) = maximum3 . map (sum . map read . lines) . splitWithStr "\n\n" $ l

------------------------------------------------------------

maximum3 :: Ord a => [a] -> (a, a, a)
maximum3 (a:b:c:xs) = foldr iter (a,b,c) xs
    where iter x (a,b,c)
            | x < c     = (a,b,c)
            | x < b     = (a,b,x)
            | x < a     = (a,x,b)
            | otherwise = (x,a,b)

splitWithStr :: Eq a => [a] -> [a] -> [[a]]
splitWithStr x y = func x y [[]]
    where
        func x [] z = reverse $ map reverse z
        func x (y:ys) (z:zs) = if take (length x) (y:ys) == x then
            func x (drop (length x) (y:ys)) ([]:(z:zs))
        else
            func x ys ((y:z):zs)