import Data.List

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x >= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x >= y
valid Div x y = y /= 1 && x `mod` y == 0
valid Exp x y = False {- Disable Exp -}

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
                    where
                     brak (Val n) = show n
                     brak e = "(" ++ show e ++ ")"

-- Order Exprs by total number of operands
len :: Expr -> Int
len (Val n) = 1
len (App o l r) = len l + len r

instance Eq Expr where
  e == e' = len e == len e'

instance Ord Expr where
  e <= e' = len e <= len e'

values :: Expr -> [Int]
values (Val n) = [n]
values (App o l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) yss ++ yss where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices ns = [c | ss <- subs ns, c <- perms ss]
-- Alternatively,
-- choices :: [a] -> [[a]]
-- choices = concat . map perms . subs

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst x [] = []
removeFirst x (y:ys) | x == y = ys
                     | otherwise = y:removeFirst x ys

isChoices :: Eq a => [a] -> [a] -> Bool
isChoices [] ys = True
isChoices (x:xs) [] = False
isChoices xs (y:ys) = isChoices (removeFirst y xs) ys

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div, Exp]]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | c <- choices ns, e <- exprs c, eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
                    l <- results ls,
                    r <- results rs,
                    res <- combine' l r]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y)
                        | o <- [Add, Sub, Mul, Div, Exp], valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

nearest :: [Int] -> Int -> [Expr]
nearest ns n = sort [e | (e,m) <- rs, abs (m-n) == d]
              where rs = [r | ns' <- choices ns, r <- results ns']
                    d = minimum (map (abs . subtract n . snd) rs)
