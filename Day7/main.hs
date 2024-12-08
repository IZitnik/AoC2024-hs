module Main where
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))

data Eqt = Eqt Int   -- Result
               [Int] -- To combine
            deriving (Show)
type Op = Int -> Int -> Int


parseEq :: String -> [Eqt]
parseEq = map parseLine.lines
  where
    parseLine :: String -> Eqt
    parseLine line = let (left,_:_:right) = span (/= ':') line
                     in Eqt (read left :: Int) (map read $ words right)

ccat :: Int -> Int -> Int
ccat a b = a*(10^cif b) + b
  where cif !x = 1 + if abs x >= 10 then cif (x `div` 10) else 0

getSolvable :: [Op] -> Eqt -> Maybe Int
getSolvable operators (Eqt res toCombine) = if reduce' toCombine 0
  then Just res
  else Nothing
    where
      reduce' :: [Int] -> Int -> Bool
      reduce' []     s = s == res
      reduce' (x:xs) s
        | s >= 0       = or [reduce' xs (s `f` x) | f <- operators]
        | otherwise    = False

part1 :: [Eqt] -> Int
part1 = sum . mapMaybe (getSolvable [(+), (*)])

part2 :: [Eqt] -> Int
part2 = sum . mapMaybe (getSolvable [(+), (*), ccat])


main :: IO ()
main = do equations <- readFile "input.txt" <&> parseEq
          print . (part1 &&& part2) $ equations
