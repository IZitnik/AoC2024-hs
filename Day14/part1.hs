module Main where
import qualified Data.Map as M
import Data.Char (ord, chr)
newtype Vec2 = Vec2 (Int, Int) deriving (Show, Eq, Ord)
type Pos     = Vec2
type Vel     = Vec2
type Bounds  = Vec2
data Robot   = Robot Pos Vel deriving (Show, Eq)
data Board   = Board Bounds [Robot]
instance Num Vec2 where
  (+) :: Vec2 -> Vec2 -> Vec2
  (Vec2 (lx,ly)) + (Vec2 (rx,ry)) = Vec2 (lx+rx, ly+ry)

  (*) :: Vec2 -> Vec2 -> Vec2
  (Vec2 (lx,ly)) * (Vec2 (rx,ry)) = Vec2 (lx*rx, ly*ry)

  abs :: Vec2 -> Vec2
  abs (Vec2 (x,y)) = Vec2 (abs x, abs y)

  negate :: Vec2 -> Vec2
  negate (Vec2 (x,y)) = Vec2 ((-x), (-y))

-- ----
debug = True

position, velocity :: Robot -> Vec2
position (Robot p _) = p
velocity (Robot _ v) = v

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn _   []                 = undefined
splitOn sep (x:xs) | x == sep  = ([], xs)
                   | otherwise = let (l, r) = splitOn sep xs
                                  in (x:l, r)

parse :: String -> [Robot]
parse = (map parseRobot) . lines
  where parseRobot :: String -> Robot
        parseRobot inp = let (l,r) = splitOn ' ' inp
                          in Robot (Vec2 $ valParse l) (Vec2 $ valParse r)
        valParse :: String -> (Int, Int)
        valParse = (\(a,b)->(read a, read b)) . (splitOn ',') . (drop 2)


move :: Bounds -> Robot -> Robot
move b (Robot p v) = Robot (wib (p+v)) v
  where wib = wrapInBounds b

updateBoard :: Board -> Board
updateBoard (Board b r) = Board b $ map (move b) r

solve :: Board -> Int
solve initial = calcSafety $ (iterate updateBoard initial) !! 100
  where calcSafety :: Board -> Int
        calcSafety = undefined

wrapInBounds :: Bounds -> Vec2 -> Vec2
wrapInBounds (Vec2 (bx, by)) (Vec2 (x, y)) = Vec2 (adj x bx, adj y by)
  where adj v b = if v<0
                    then v+b
                    else if v>=b
                      then v-b
                      else v

fromJust :: Maybe a -> a
fromJust Nothing  = undefined
fromJust (Just a) = a

intToChr :: Int -> Char
intToChr n = chr $ (mod n 10) + ord '0'

displayBoard :: Board -> String
displayBoard (Board (Vec2 (bx,by)) r) = let m = bToM r
                                         in unlines [[look m $ Vec2 (x,y) | x<-[0..bx]]
                                                                          | y<-[0..by]]
  where bToM :: [Robot] -> M.Map Vec2 Int
        bToM = (foldr (M.alter go) M.empty) . (map position)
        go :: Maybe Int -> Maybe Int
        go Nothing  = Just $ 1
        go (Just n) = Just $ 1+n
        look :: M.Map Vec2 Int -> Vec2 -> Char
        look m v | M.member v m = intToChr $ fromJust $ M.lookup v m
                 | otherwise    = '.'

main :: IO ()
main = do f_content <- readFile $ if debug then "test.txt" else "input.txt"
          let robots = parse f_content
          let bounds = Vec2 $ if debug then (10,6) else (100,102)
          let board  = Board bounds robots
          putStrLn $ displayBoard board
          putStrLn $ displayBoard $ (iterate updateBoard board) !! 100
          print $ solve board

