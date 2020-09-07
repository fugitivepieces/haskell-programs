module Utils where

import System.IO

newline :: IO ()
newline = putChar '\n'

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

