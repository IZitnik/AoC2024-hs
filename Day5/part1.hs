module Main where
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Read (readMaybe)
type Input = [Integer]          -- Pages
type OrdTable = M.Map Integer   -- Page ID
                      [Integer] -- Page ID list shouldn't appear before

-- Parse:
--  Dictionary key: page id, val: list které se nesmí objevit
-- Solve:
--  Track seen, lookup cur pokud shoda s čímkoliv v seen, Fail
--  otherwise, přidat do seen a pro další value
--  Correct pro []

ordParse :: [String] -> OrdTable
ordParse = foldl go M.empty
  where go :: OrdTable -> String -> OrdTable
        go map rule = let (x,y) = (\(l,r)->(toInt l, toInt r)) $ splitFirst '|' rule
                       in M.alter ((\x->Just x) . (maybe [y] (y:))) x map

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
    where go [] acc = [reverse acc]
          go (y : ys) acc = if x == y
                            then reverse acc : go ys []
                            else go ys (y : acc)

splitFirst :: Eq a => a -> [a] -> ([a],[a])
splitFirst el lst = let (l, r) = break (==el) lst in (l, tail r)

toInt :: String -> Integer
toInt inp = case readMaybe inp :: Maybe Integer of
              (Just n)  -> n
              (Nothing) -> error ("'" ++ inp ++ "'")

parse :: String -> (OrdTable,[Input])
parse = fn' . (\(l,r)->(l,init r)) . splitFirst "" . lines
  where fn' :: ([String], [String]) -> (OrdTable, [Input])
        fn' (l,r) = (ordParse l, map ((map toInt) . splitOn ',') r)

solve :: Input -> OrdTable -> Bool
solve inp ord = s' inp S.empty
  where s' :: Input -> S.Set Integer -> Bool
        s' []     _    = True
        s' (x:xs) seen = case M.lookup x ord of
                           (Just pre) -> noElInM seen pre && s' xs (S.insert x seen)
                           (Nothing)  -> s' xs (S.insert x seen)

noElInM :: Ord a => S.Set a -> [a] -> Bool
noElInM s = all (flip S.notMember s)

middle :: [a] -> a
middle lst = lst !! (length lst `div` 2)

main :: IO ()
main = do input <- readFile "input.txt"
          let (ordT, inp) = parse $ input
          print $ sum $ map middle $ filter ((flip solve) ordT) inp
