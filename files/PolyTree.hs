data Tree a = Leaf | Node a (Tree a) (Tree a)

isSorted :: Ord a => Tree a -> Bool
isSorted = everyNode $ \y t1 t2 ->
    everyValue (\x -> x <= y) t1 &&
    everyValue (\z -> y <= z) t2

everyValue :: (a -> Bool) -> Tree a -> Bool
everyValue p = everyNode (\x _ _ -> p x)

everyNode :: (a -> Tree a -> Tree a -> Bool) -> Tree a -> Bool
everyNode p (Node x t1 t2) = p x t1 t2 && everyNode p t1 && everyNode p t2
everyNode _ Leaf = True

data ABC = A | B | C deriving Eq
instance Ord ABC where
  x <= y | x == y = True
  A <= B = True
  B <= C = True
  _ <= _ = False

