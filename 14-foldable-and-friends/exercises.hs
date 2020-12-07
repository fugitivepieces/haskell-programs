import Data.Monoid

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

instance Foldable Maybe where
  -- fold :: Monoid a => Maybe a -> a
  fold Nothing = mempty
  fold (Just x) = x

  -- foldMap :: Monoid b => (a -> b) -> t a -> b
  foldMap _ Nothing = mempty
  foldMap g (Maybe x) = g x

  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr _ v Nothing = v
  foldr f v (Maybe x) = f x v

  -- foldl :: (b -> a -> b) -> b -> t a -> b
  foldl _ v Nothing = v
  foldl f v (Maybe x) = f v x

instance Traversable Maybe where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
