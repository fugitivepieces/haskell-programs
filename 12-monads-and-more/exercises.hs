data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

{- Create a newtype Ntype that wraps the partially-applied
 - function type to avoid duplicate definition with GHC.Base -}
data Ntype a b = N (a -> b)
appN :: (Ntype a b) -> a -> b
appN (N f) = f

instance Functor (Ntype a) where
  -- fmap :: (b -> c) -> N (a -> b) -> N (a -> c)
  fmap f (N g) = N (f . g)

instance Applicative (Ntype a) where
  -- pure :: b -> (a -> b)
  pure y = N (\x -> y)

  -- <*> :: Ntype a (b -> c) -> Ntype a b -> Ntype a c
  N f <*> N g = N (\x -> f x (g x))

instance Monad (Ntype a) where
  -- (>>=) :: N (a -> b) -> (b -> N (a -> c)) -> N (a -> c)
  N f >>= g = N (\x -> appN (g (f x)) x)

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> Z a -> Z b
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)

  -- <*> ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var x) = Var (f x)
  fmap _ (Val n) = Val n
  fmap f (Add x y) = Add (fmap f x) (fmap f y)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure x = Var x

  -- <*> :: Expr (a -> b) -> Expr a -> Expr b
  Var f <*> x = fmap f x

instance Monad Expr where
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Var x >>= f = f x
  Val n >>= _ = Val n
  Add x y >>= f = Add (x >>= f) (y >>= f)

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f st = do x <- st
                 return (f x)

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- <*> :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do f <- stf
                   x <- stx
                   return (f x)

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')


