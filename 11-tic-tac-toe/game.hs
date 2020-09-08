import Data.Char
import Data.List
import System.IO

size::Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
          where os = length (filter (== O) ps)
                xs = length (filter (== X) ps)
                ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
            where line = all (== p)
                  rows = g
                  cols = transpose g
                  dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [1..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
            beside = foldr1 (zipWith (++))
            bar = replicate 3 "|"

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer X = ["   ", " X ", "   "]
showPlayer B = ["   ", "   ", "   "]

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> Grid
move g n p = if valid g n then (chop size (xs ++ [p] ++ ys)) else []
             where (xs, B:ys) = splitAt n (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                     return (read xs)
                   else
                     do putStrLn "ERROR: Invalid number"
                        getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)
goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!"
        | wins X g = putStrLn "Player X wins!"
        | full g   = putStrLn "It's a draw!"
        | otherwise =
           do i <- getNat (prompt p)
              case move g i p of 
                [] -> do putStrLn "Error: Invalid move"
                         run' g p
                g' -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a]
              deriving Show

numNodes :: Tree a -> Int
numNodes (Node _ xs) = 1 + sum (map numNodes xs)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
    | won g     = []
    | full g    = []
    | otherwise = filter (/= []) [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9
