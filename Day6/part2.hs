module Main where
import Data.Set (Set, empty, insert, size, member)
type Pos       = (Int, Int)
type Bounds    = (Int, Int)
type Guard     = Pos
type Obst      = Pos
data Direction = North | East | South | West deriving (Show)
data Parsed    = Parsed Bounds Guard (Set Obst) deriving (Show)

getBounds :: String -> Bounds
getBounds str = let ln@(h:_) = lines str in ((length h)-1, (length ln)-1)

parse :: String -> Parsed
parse inp = foldl (\p (l,n)->foldl (go n) p $ zip l [0..])
                  (Parsed (getBounds inp) (0,0) empty)
                  (zip (lines inp) [0..])
  where go :: Int -> Parsed -> (Char,Int) -> Parsed
        go row (Parsed b g o) (ch,col)
           | ch == '.'  = Parsed b g o
           | ch == '#'  = Parsed b g $ insert (row,col) o
           | ch == '^'  = Parsed b (row,col) o
           | otherwise = error "Unexpected token encountered in parse"

next :: Pos -> Direction -> Pos
next (y,x) North = (y-1, x  )
next (y,x) East  = (y  , x+1)
next (y,x) South = (y+1, x  )
next (y,x) West  = (y  , x-1)

right :: Direction -> Direction
right North = East
right East  = South
right South = West
right West  = North

solve :: Bounds -> Guard -> Set Obst -> Int
solve b g o = size $ solve' b g o North empty

solve' :: Bounds -> Guard -> Set Obst -> Direction -> Set Pos -> Set Pos
solve' b@(bx,by) g@(x,y) obs dir n =
  if x>=0 && x<=bx && y>=0 && y<=by
    then if member (next g dir) obs
      then solve' b g            obs (right dir) n             -- Obstacle, 90° right
      else solve' b (next g dir) obs dir         (insert g n)  -- No Obst, Mov in dir
    else n -- Guard out of bounds, END

joinWith :: String -> [String] -> String
joinWith sep = foldl1 (\a b-> a++sep++b)

getBoard :: Bounds -> Set Obst -> Set Pos -> String
getBoard (bx,by) o t = joinWith "\n" [[gc (y,x) | x<-[0..bx]] | y<-[0..by]]
  where gc :: Pos -> Char
        gc p | member p o && member p t = error "Traversed throught obstacle"
             | member p o = '#'
             | member p t = 'X'
             | otherwise  = '.'

main :: IO ()
main = do f_content <- readFile "test.txt"
          let (Parsed bounds guard obsts) = parse f_content
          let traversed = solve' bounds guard obsts North empty
          putStrLn $ getBoard bounds obsts traversed
          print $ size traversed

