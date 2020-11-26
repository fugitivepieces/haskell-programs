import System.IO
import Utils
import Parser

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: String
buttons = standards ++ extra
            where standards = "qcd=123+456-789*0()/"
                  extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

display xs = do writeat (3,2) (replicate 13 ' ')
                writeat (3,2) (reverse (take 13 (reverse xs)))

display_below xs = do writeat (1,14) xs

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
               process c xs
             else
               do beep
                  calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC" = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n" = eval xs
             | elem c "cC" = clear
             | otherwise = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case (parse expr xs) of
            [(n,[])] -> do writeat (1,14) (replicate 13 ' ')
                           calc (show n)
            [(_,ys)] -> do beep
                           display_below ("Error at: " ++ show (length xs - length ys))
                           calc xs
            [] -> do beep
                     display_below "Error at: 0"
                     calc xs

clear :: IO ()
clear = calc ""

press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear
