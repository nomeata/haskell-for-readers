data Tree = Leaf | Node Integer Tree Tree

insert :: Integer -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node y t1 t2)
    | y < x     = Node y t1 (insert x t2)
    | otherwise = Node y (insert x t1) t2

