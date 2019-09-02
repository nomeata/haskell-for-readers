import Prelude hiding (fmap, (<$>))
(>>) :: Monad m => m a -> m b -> m b
a >> b = a >>= (\_ -> b)
fmap :: Monad f => (a -> b) -> f a -> f b
fmap f a = a >>= (return . f)
(<$>) :: Monad f => (a -> b) -> f a -> f b
(<$>) = fmap
(<$) :: Monad f => a -> f b -> f a
x <$ a = const x <$> a
liftA2 :: Monad f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = a >>= (\x -> b >>= (\y -> return (f x y)))
(<*>) :: Monad f => f (a -> b) -> f a -> f b
(<*>) = liftA2 (\f x -> f x)
(<*) :: Monad f => f a -> f b -> f a
(<*) = liftA2 (\x y -> x)
(*>) :: Monad f => f a -> f b -> f b
(*>) = liftA2 (\x y -> y)
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
a >=> b = (\x -> a x >>= b)
join :: Monad m => m (m a) -> m a
join a = a >>= id

