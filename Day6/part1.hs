module Main where
import Data.Set (Set, empty, insert, size, member)
type Pos       = (Int, Int)
type Bounds    = (Int, Int)
type Obst      = Pos
data Direction = North | East | South | West deriving (Show, Eq, Enum)
data Guard     = Guard Pos Direction deriving (Show, Eq)
data Parsed    = Parsed Bounds Guard (Set Obst) deriving (Show)

getBounds :: String -> Bounds
getBounds str = let ln@(h:_) = lines str in ((length h)-1, (length ln)-1)

parse :: String -> Parsed
parse inp =
  let bounds  = getBounds inp
      guard   = Guard (0,0) North
      initial = Parsed bounds guard empty
   in foldl parseLine initial $ zip (lines inp) [0..]
  where parseLine :: Parsed -> (String, Int) -> Parsed
        parseLine p (l,n) = foldl (parseChar n) p $ zip l [0..]
        parseChar :: Int -> Parsed -> (Char, Int) -> Parsed
        parseChar row (Parsed b g o) (ch,col)
           | ch == '.'  = Parsed b g o
           | ch == '#'  = Parsed b g $ insert pos o
           | ch == '^'  = Parsed b (Guard pos North) o
           | otherwise = error "Unexpected token encountered in parse"
            where pos = (row, col)

move :: Guard -> Guard
move (Guard (y,x) dir) =
  case dir of
    North -> Guard (y-1, x  ) dir
    East  -> Guard (y  , x+1) dir
    South -> Guard (y+1, x  ) dir
    West  -> Guard (y  , x-1) dir

rotate :: Guard -> Guard
rotate (Guard p dir) = Guard p (right dir)
  where right :: Direction -> Direction
        right West = North
        right dir  = succ dir

position :: Guard -> Pos
position (Guard p _) = p

inBounds :: Bounds -> Pos -> Bool
inBounds (bx,by) (x,y) = x>=0 && x<=bx && y>=0 && y<=by

solve :: Bounds -> Guard -> Set Obst -> Set Pos
solve b g obs = foldl (\b a -> insert (position a) b) empty $ iterateWhile inB step' g
  where inB :: Guard -> Bool
        inB = inBounds b . position
        step' :: Guard -> Guard
        step' = step obs

iterateWhile :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateWhile end fn init = takeWhile end $ iterate fn init

step :: Set Obst -> Guard -> Guard
step obs guard
  | obstacleAhead = rotate guard
  | otherwise     = move   guard
   where obstacleAhead :: Bool
         obstacleAhead = member (position . move $ guard) obs

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
          let traversed = solve bounds guard obsts
          -- putStrLn $ getBoard bounds obsts traversed
          print $ size traversed

