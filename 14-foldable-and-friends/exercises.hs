import Data.Monoid
import Control.Conditional (select)

-- Duplicate instance declarations:
--       instance (Monoid a, Monoid b) => Monoid (a, b)
--         -- Defined at exercises.hs:3:10
--       instance (Monoid a, Monoid b) => Monoid (a, b)
--         -- Defined in ‘GHC.Base’

-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--   -- mempty :: (a,b)
--   mempty = (mempty, mempty)

--   -- mappend :: (a,b) -> (a,b) -> (a,b)
--   (x,y) `mappend` (x',y') = (x `mappend` x', y mappend y')

-- Duplicate instance declarations:
--       instance Monoid b => Monoid (a -> b)
--         -- Defined at exercises.hs:16:10
--       instance Monoid b => Monoid (a -> b) -- Defined in ‘GHC.Base’

-- instance (Monoid b) => Monoid (a -> b) where
--   -- mempty :: a -> b
--   mempty _ = mempty

--   -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
--   mappend f g x = (f x) `mappend` (g x)

fold :: Monoid a => Maybe a -> a
fold Nothing = mempty
fold (Just x) = x

-- Duplicate instance declarations:
--   instance Foldable Maybe -- Defined at exercises.hs:32:10
--   instance Foldable Maybe -- Defined in ‘Data.Foldable’

-- instance Foldable Maybe where
--   -- foldMap :: Monoid b => (a -> b) -> t a -> b
--   foldMap _ Nothing = mempty
--   foldMap g (Just x) = g x

--   -- foldr :: (a -> b -> b) -> b -> t a -> b
--   foldr _ v Nothing = v
--   foldr f v (Just x) = f x v

--   -- foldl :: (b -> a -> b) -> b -> t a -> b
--   foldl _ v Nothing = v
--   foldl f v (Just x) = f v x

-- Duplicate instance declarations:
--       instance Traversable Maybe -- Defined at exercises.hs:45:10
--       instance Traversable Maybe -- Defined in ‘Data.Traversable’

-- instance Traversable Maybe where
--   -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
--   traverse _ Nothing = pure Nothing
--   traverse g (Just x) = Just <$> g x

-- exercises.hs:59:10: error:
--     • No instance for (Functor Tree)
--         arising from the superclasses of an instance declaration
--     • In the instance declaration for ‘Traversable Tree’

-- exercises.hs:59:10: error:
--     • No instance for (Foldable Tree)
--         arising from the superclasses of an instance declaration
--     • In the instance declaration for ‘Traversable Tree’

-- data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show
-- instance Traversable Tree where
--   -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
--   traverse _ Leaf = pure Leaf
--   traverse g (Node l x r) = Node <$> traverse g l <*> g x <*> traverse g r

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = foldMap (select f pure mempty)
