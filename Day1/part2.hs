module Main where
import Data.List
import Text.Parsec
import Text.Parsec.String

aocParse :: String -> Either ParseError [(Int,Int)]
aocParse = parse aocFile "(unknown)"

aocFile = endBy line eol

line :: GenParser Char st (Int, Int)
line = do d1 <- many digit
          many (oneOf " \t")
          d2 <- many digit
          return (read d1, read d2)

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"


sumDiff :: [(Int,Int)] -> Int
sumDiff [] = 0
sumDiff ((x,y):xs) = p x y + sumDiff xs
  where p :: Int -> Int -> Int
        p a b | a > b     = a - b
              | b > a     = b - a
              | otherwise = 0

main =
    do content <- readFile "input.txt"
       case parse aocFile "(stdin)" content of
         Left e -> print e
         Right out -> let (a,b) = unzip out
                      in print $ sumDiff $ zip (sort a) (sort b)


