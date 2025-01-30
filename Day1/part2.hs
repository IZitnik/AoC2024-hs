module Main where
import Data.List

parse :: String -> [(Int, Int)]
parse = convert . (map words) . lines
    where convert :: [[String]] -> [(Int,Int)]
          convert = map (\[l,r] -> (read l :: Int, read r :: Int))

count :: Eq a => a -> [a] -> Int
count n = foldl (\b a -> if a == n then b+1 else b) 0

getSimiliarity :: [Int] -> [Int] -> [Int]
getSimiliarity []     _  = []
getSimiliarity (x:xs) l2 = ((count x l2)*x) : getSimiliarity xs l2

solve :: [(Int, Int)] -> Int
solve = sum . (uncurry getSimiliarity) . unzip

main :: IO ()
main = do input <- readFile "input.txt"
          print $ solve $ parse input

