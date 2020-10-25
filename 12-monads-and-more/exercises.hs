data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

{- Create a newtype Ntype that wraps the partially-applied
 - function type to avoid duplicate definition with GHC.Base -}
data Ntype a b = N (a -> b)
app :: (Ntype a b) -> a -> b
app (N f) = f

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
  N f >>= g = N (\x -> app (g (f x)) x)

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
  fmap f (Val n) = Val n
  fmap f (Add x y) = Add (fmap f x) (fmap f y)
