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
  pure x = (\s -> [(x, s)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = 
