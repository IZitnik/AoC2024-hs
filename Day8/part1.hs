module Main where
import Data.Map (Map, empty, alter, fromList, lookup)
import qualified Data.Set as S
type Antenna = (Int, Int)
type Anode   = (Int, Int)
type Bounds  = (Int, Int)
data Parsed  = Parsed (Map Char [Antenna]) Bounds

getBounds :: String -> Bounds
getBounds str = let ln@(h:_) = lines str in ((length h)-1, (length ln)-1)

inBounds :: Bounds -> (Int, Int) -> Bool
inBounds (bx, by) (x, y) = x>=0 && x<=bx && y>=0 && y<=by

parse :: String -> Parsed
parse inp =
  let initial = Parsed empty (getBounds inp)
   in foldl parseLine initial $ zip (lines inp) [0..]
  where parseLine :: Parsed -> (String, Int) -> Parsed
        parseLine p (line,num) = foldl (parseChar num) p $ zip line [0..]
        parseChar :: Int -> Parsed -> (Char, Int) -> Parsed
        parseChar row (Parsed m b) (ch,col)
           | ch == '.'  = Parsed m               b
           | otherwise  = Parsed (alter go ch m) b
            where go :: Maybe [Antenna] -> Maybe [Antenna]
                  go Nothing  = Just $ [(row, col)]
                  go (Just a) = Just $ (row, col) : a

solve :: Map Char [Antenna] -> Bounds -> S.Set Anode
solve m b = foldl go S.empty m
  where go :: S.Set Anode -> [Antenna] -> S.Set Anode
        go s a = S.union s (S.fromList $ filter (inBounds b) $ findAntiFreqs a)

findAntiFreqs :: [Antenna] -> [Anode]
findAntiFreqs an = foldl go [] an
  where go :: [Anode] -> Antenna -> [Anode]
        go b c = [getAnode c x | x<-an, x/=c] ++ b

getAnode :: Antenna -> Antenna -> Anode
getAnode (sx,sy) (dx,dy) = (fn sx dx, fn sy dy)
  where fn :: Int -> Int -> Int
        fn s d = s + (s-d)

draw :: Map Char [Antenna] -> S.Set Anode -> Bounds -> String
draw m a (bx, by) = unlines [[gc (y,x) | x<-[0..bx]] | y<-[0..by]]
  where gc :: (Int, Int) -> Char
        gc p | S.member p a = '#'
             | otherwise  = '.'

main :: IO ()
main = do f_content <- readFile "input.txt"
          let (Parsed m bounds) = parse f_content
          putStrLn $ draw m (solve m bounds) bounds
          print $ S.size $ solve m bounds
