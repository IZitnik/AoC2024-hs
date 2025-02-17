module Main where
import Control.Monad.State
import Data.Maybe (isJust)
import Data.Map (Map, empty, insert, fromList)
import qualified Data.Map as M
type Stone  = Int
type Cache  = Map Stone Int

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

addCache :: Stone -> Int -> State Cache Int
addCache k v = do m <- get
                  put $ insert k v m

inCache :: Stone -> State Cache (Maybe Int)
inCache k = do m <- get
               return $ M.lookup k m


stoneCnt :: Stone -> Int -> State Cache Int
stoneCnt _ 0    = return 1
stoneCnt n step = undefined

solve :: Int -> [Int] -> Int
solve n = sum . (map fst) . (map $ (\x->runState (stoneCnt n x) empty))

main :: IO ()
main = do f_content <- readFile "input.txt"
          let inp = parse f_content
          print inp
          print $ solve 75 inp
