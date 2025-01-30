module Main where

parse :: String -> [[Int]]
parse = map (map toInt . words) . lines where toInt n = read n :: Int

diffs :: [Int] -> [Int]
diffs lst = map (\(a,b)->a-b) $ zip lst (tail lst)

sameDir :: [Int] -> Bool
sameDir lst = all (<0) lst || all (>0) lst

between :: Int -> Int -> [Int] -> Bool
between l h = and . map (\x->x>l && x<h)

isSafe :: [Int] -> Bool
isSafe lst = let d = diffs lst in sameDir d && (between 0 4 $ map abs d)

solve :: [[Int]] -> Int
solve = (foldl (\b a->if a then b+1 else b) 0) . (map isSafe)

main :: IO ()
main = do input <- readFile "input.txt"
          print $ solve $ parse input
