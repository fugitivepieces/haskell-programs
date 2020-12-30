-- mult = \x -> (\y -> x * y)

-- mult 3 4 = (\x -> (\y -> x * y)) 3 4
--          = (\y -> 3 * y) 4
--          = 3 * 4
--          = 12

fibs :: [Integer]
fibs = 0:(1:fibs')
        where fibs' = map (\(x,y) -> x + y) (zip fibs (tail fibs))

data Tree a = Leaf | Node (Tree a) a (Tree a)
                deriving Show
repeat' :: a -> Tree a
repeat' x = Node t x t where t = repeat' x

take' :: Int -> Tree a -> Tree a
take' 0 _ = Leaf
take' _ Leaf = Leaf
take' n (Node l x r) | n `mod` 2 == 1 = Node (take' n2 l) x (take' n2 r)
                     | otherwise = Node (take' n2 l) x (take' (n2-1) r)
                    where n2 = n `div` 2

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat'

sqroot :: Double -> Double
sqroot x = sqroot' (iterate (\y -> (y + x / y) / 2) 1)

sqroot' :: [Double] -> Double
sqroot' (x:xs) = if abs (x - head xs) < 0.00001 then x else sqroot' xs
