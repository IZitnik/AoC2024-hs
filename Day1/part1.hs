module Main where
import Data.List

parse :: String -> [(Int, Int)]
parse = convert . (map words) . lines
    where convert :: [[String]] -> [(Int,Int)]
          convert = map (\[l,r] -> (read l :: Int, read r :: Int))

sumDiff :: [(Int,Int)] -> Int
sumDiff [] = 0
sumDiff ((x,y):xs) = p x y + sumDiff xs
  where p :: Int -> Int -> Int
        p a b | a > b     = a - b
              | b > a     = b - a
              | otherwise = 0

zipApply :: ([a] -> [b]) -> [(a,a)] -> [(b,b)]
zipApply fn lst = let (l,r) = unzip lst
                   in zip (fn l) (fn r)

main :: IO ()
main = do input <- readFile "input.txt"
          print $ sumDiff $ zipApply sort $ parse input

