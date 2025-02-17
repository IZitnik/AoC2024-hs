module Main where

parse :: String -> [Int]
parse = (map read) . (split ' ')

split :: Eq a => a -> [a] -> [[a]]
split split = foldr (go split) [[]]
  where go :: Eq a => a -> a -> [[a]] -> [[a]]
        go s e b | e == s =  [] : b
                 | e /= s =  (e : (head b)) : (tail b)

digits :: Int -> Int
digits 0 = 0
digits n = 1 + (digits $ div n 10)

digSplit :: Int -> (Int, Int)
digSplit n = divMod n (10^(div (digits n) 2))

blink :: [Int] -> [Int]
blink = foldl go [] . reverse
  where go :: [Int] -> Int -> [Int]
        go b 0 = 1 : b
        go b a | even $ digits a = let (l,r) = digSplit a in l : r : b
               | otherwise       = a*2024 : b

stones :: [Int] -> [[Int]]
stones = iterate blink

main :: IO ()
main = do f_content <- readFile "input.txt"
          let inp = parse f_content
          print inp
          print $ length $ (stones inp) !! 25
