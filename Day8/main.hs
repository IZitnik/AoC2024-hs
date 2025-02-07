module Main where
import Control.Arrow ((&&&))
import Data.Functor  ((<&>))
import Data.Map

type Position = (Int, Int)
data Board = Board (Int, Int)              -- Bounds
                   (Map String [Position]) -- Map[antenna, positions]

parseData :: [String] -> Board
parseData input = Board (length $ head input, length input) (generateMap empty)
  where
    generateMap :: Map String [Position] -> Map String [Position]
    generateMap m = undefined

part1 :: a -> Int
part1 = undefined

part2 :: a -> Int
part2 = undefined

main :: IO ()
main = do equations <- readFile "input.txt" <&> parseData . lines
          print . (part1 &&& part2) $ equations
