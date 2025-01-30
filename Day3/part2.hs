module Main where
import Text.Parsec (notFollowedBy, try, (<?>), anyToken, runParser)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, digit, anyChar, string)
import Control.Applicative (many, (<|>))
data Ins = Mul Int Int | Do | Dont deriving (Show)

parse :: Parser [Ins]
parse = many $ try (skipUntil parseIns)

skipUntil :: Parser a -> Parser a
skipUntil p = try p <|> (anyChar >> skipUntil p)

parseIns :: Parser Ins
parseIns = (try parseDont) <|> (try parseDo) <|> (try parseMul)

parseMul :: Parser Ins
parseMul = do string "mul("
              d1 <- many digit
              char ','
              d2 <- many digit
              char ')'
              pure (Mul (read d1 :: Int) (read d2 :: Int))

parseDo :: Parser Ins
parseDo = do string "do()"
             pure Do

parseDont :: Parser Ins
parseDont = do string "don't()"
               pure Dont

eval :: [Ins] -> Int
eval inp = eval' inp True
  where eval' :: [Ins] -> Bool -> Int
        eval' []             _     = 0
        eval' ((Do):xs)      _     = eval' xs True
        eval' ((Dont):xs)    _     = eval' xs False
        eval' ((Mul a b):xs) True  = a * b + (eval' xs True)
        eval' ((Mul a b):xs) False = eval' xs False

main :: IO ()
main = do input <- readFile "input.txt"
          case runParser parse () "(stdin)" input of
              (Left  err) -> print err >>= error "Failed to parse"
              (Right res) -> do print res
                                print $ eval res
