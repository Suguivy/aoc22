import Data.List.Split
import Data.List
import qualified Data.HashMap as M

main :: IO ()
main = do
    contents <- readFile "input"
    putStrLn $ "Part 1: " ++ part1 contents
    putStrLn $ "Part 2: " ++ part2 contents

part1 :: String -> String
part1 = show . sum . filter (<=100000) . M.elems . readAndGetDirSizes

part2 :: String -> String
part2 s = show . head . dropWhile ((<30000000) . (+unusedSpace)) . sort . M.elems $ dirSizes
    where unusedSpace = 70000000 - (dirSizes M.! "/")
          dirSizes = readAndGetDirSizes s

readAndGetDirSizes :: String -> M.Map String Integer
readAndGetDirSizes = snd . foldl readIns ([], M.empty) . map (splitOn " ") . lines
    where readIns (dirs, m) ["$",   "cd", ".."] = (init dirs, m)
          readIns (dirs, m) ["$",   "cd", "/"]  = (["/"], m)
          readIns (dirs, m) ["$",   "cd", name] = (dirs ++ [name], m)
          readIns (dirs, m) ["$",   "ls"]       = (dirs, m)
          readIns (dirs, m) ["dir", name]       = (dirs, m)
          readIns (dirs, m) [size,  name]       = (dirs, M.unionWith (+) m (M.fromList $ map (, read size) (scanl1 (\a x -> a ++ "/" ++ x) dirs)))