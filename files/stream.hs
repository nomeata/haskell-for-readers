data Stream a b
    = NeedInput (a -> Stream a b)
    | HasOutput b (Stream a b)
    | Done

rle :: Eq a => Stream a (Integer,a)
rle = NeedInput rle_start

rle_start :: Eq a => a -> Stream a (Integer, a)
rle_start x = NeedInput (rle_count x 1)

rle_count :: Eq a => a -> Integer -> a -> Stream a (Integer, a)
rle_count x n x' | x == x' = NeedInput (rle_count x (n + 1))
                 | otherwise = HasOutput (n, x) (rle_start x')

