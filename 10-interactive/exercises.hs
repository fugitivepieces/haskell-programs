import Utils

adder :: IO ()
adder = do putStr "How many numbers? "
           n <- getLine
           total (read n :: Int) 0

total :: Int -> Int -> IO ()
total n x = if n == 0 then
              do putStr "The total is "
                 putStrLn (show x)
            else
              do m <- getLine
                 total (n - 1) (x + read m :: Int)

adder' :: IO ()
adder' = do putStr "How many numbers? "
            n <- getLine
            xs <- sequence [getLine | _ <- [1..(read n :: Int)]]
            putStr "The total is "
            putStrLn (show (sum [read y :: Int | y <- xs]))

erase :: IO ()
erase = putStr "\b \b"

getLineWithDelete :: IO String
getLineWithDelete = do x <- getCh
                       case x of '\n' -> do putChar '\n'
                                            return []
                                 '\DEL' -> do erase
                                              xs <- getLineWithDelete
                                              return (x:xs)
                                 _ -> do putChar x
                                         xs <- getLineWithDelete
                                         return (x:xs)

removeErased :: String -> String
removeErased = foldl (\xs x -> if x == '\DEL' then init xs else xs ++ [x]) ""

readLine :: IO String
readLine = do xs <- getLineWithDelete
              return (removeErased xs)
