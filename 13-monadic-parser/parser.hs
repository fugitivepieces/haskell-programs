module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) s = p s

item :: Parser Char
item = P (\s -> case s of
            [] -> []
            (x:xs) -> [(x, xs)])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\s -> case parse p s of
                  [] -> []
                  [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\s -> [(x, s)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\s -> case parse pg s of
                  [] -> []
                  [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- >>= Parser a -> (a -> Parser b) -> Parser b
  px >>= g = P (\s -> case parse px s of
                  [] -> []
                  [(v, out)] -> parse (g v) out)

instance Alternative Parser where
  -- empty :: Parser
  empty = P (\s -> [])

  -- <|> :: Parser a -> Parser a -> Parser a
  px <|> py = P (\s -> case parse px s of
                  [] -> parse py s
                  x:xs -> x:xs)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

upper :: Parser Char
upper = sat isUpper

lower :: Parser Char
lower = sat isLower

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = nat <|>
      do char '-'
         n <- nat
         return (-n)

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> nat

-- eval :: String -> Int
-- eval xs = case (parse expr xs) of
--             [(n, [])] -> n
--             [(_, out)] -> error ("Unused input " ++ out)
--             [] -> error "Invalid input"
