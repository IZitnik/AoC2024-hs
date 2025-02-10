module Main where
import Data.Set (Set, empty, insert, size, member)
type Pos       = (Int, Int)
type Bounds    = (Int, Int)
type Guard     = Pos
type Obst      = Pos
data Direction = North | East | South | West deriving (Show, Eq, Enum)
data Parsed    = Parsed Bounds Guard (Set Obst) deriving (Show)

getBounds :: String -> Bounds
getBounds str = let ln@(h:_) = lines str in ((length h)-1, (length ln)-1)

parse :: String -> Parsed
parse inp =
  let bounds  = getBounds inp
      initial = Parsed bounds (0,0) empty
   in foldl parseLine initial $ zip (lines inp) [0..]
  where parseLine :: Parsed -> (String, Int) -> Parsed
        parseLine p (l,n) = foldl (parseChar n) p $ zip l [0..]
        parseChar :: Int -> Parsed -> (Char, Int) -> Parsed
        parseChar row (Parsed b g o) (ch,col)
           | ch == '.'  = Parsed b g o
           | ch == '#'  = Parsed b g $ insert (row,col) o
           | ch == '^'  = Parsed b (row,col) o
           | otherwise = error "Unexpected token encountered in parse"

next :: Pos -> Direction -> Pos
next (y,x) dir = case dir of
  North -> (y-1, x  )
  East  -> (y  , x+1)
  South -> (y+1, x  )
  West  -> (y  , x-1)

right :: Direction -> Direction
right West = North
right dir  = succ dir

inBounds :: Bounds -> Pos -> Bool
inBounds (bx,by) (x,y) = x>=0 && x<=bx && y>=0 && y<=by

solve' :: Bounds -> Guard -> Set Obst -> Direction -> Set Pos
solve' b g obs dir = foldl (\b (g,_) -> insert g b) empty $ iterateWhile inB step' (g, dir)
  where inB :: (Pos, Direction) -> Bool
        inB = (inBounds b) . fst
        step' :: (Guard, Direction) -> (Guard, Direction)
        step' (tG,tDir) = step tG obs tDir

iterateWhile :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateWhile end fn init = takeWhile end $ iterate fn init

step :: Guard -> Set Obst -> Direction -> (Guard, Direction)
step g obs dir
  | obstacleAhead = (g , right dir)
  | otherwise     = (g', dir      )
   where obstacleAhead :: Bool
         obstacleAhead = member g' obs
         g' = next g dir

getBoard :: Bounds -> Set Obst -> Set Pos -> String
getBoard (bx,by) o t = unlines [[gc (y,x) | x<-[0..bx]] | y<-[0..by]]
  where gc :: Pos -> Char
        gc p | member p o && member p t = error "Traversed throught obstacle"
             | member p o = '#'
             | member p t = 'X'
             | otherwise  = '.'

main :: IO ()
main = do f_content <- readFile "input.txt"
          let (Parsed bounds guard obsts) = parse f_content
          let traversed = solve' bounds guard obsts North
          -- putStrLn $ getBoard bounds obsts traversed
          print $ size traversed

