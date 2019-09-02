class Finite a where
    size :: Integer

instance Finite Bool where size = 2
instance Finite () where size = 1
data Suit = Diamonds | Clubs | Hearts | Spades
instance Finite Suit where size = 4

instance Finite a => Finite (Maybe a) where
    size = size @a + 1
instance (Finite a, Finite b) => Finite (a,b) where
    size = size @a * size @b
instance (Finite a, Finite b) => Finite (Either a b) where
    size = size @a + size @b

instance (Finite a, Finite b) => Finite (a -> b) where
    size = size @b ^ size @a

