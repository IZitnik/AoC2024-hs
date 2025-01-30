module Main where
import Text.Parsec (notFollowedBy, try, (<?>), anyToken, runParser)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, digit, anyChar, string)
import Control.Applicative (many, (<|>))
data Ins = Mul Int Int deriving (Show)

parse :: Parser [Ins]
parse = many $ try (skipUntil parseMul)

skipUntil :: Parser a -> Parser a
skipUntil p = try p <|> (anyChar >> skipUntil p)

parseMul :: Parser Ins
parseMul = do string "mul("
              d1 <- many digit
              char ','
              d2 <- many digit
              char ')'
              pure (Mul (read d1 :: Int) (read d2 :: Int))

eval :: [Ins] -> Int
eval []             = 0
eval ((Mul a b):xs) = a * b + (eval xs)

main :: IO ()
main = do input <- readFile "input.txt"
          case runParser parse () "(stdin)" input of
              (Left  err) -> print err >>= error "Failed to parse"
              (Right res) -> do print res
                                print $ eval res
