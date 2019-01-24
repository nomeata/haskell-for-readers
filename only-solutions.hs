{-# LANGUAGE TupleSections #-}
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Data.IORef

main :: IO ()
main = toJSONFilter $ \(Pandoc meta bs) -> do
    r <- newIORef id
    let go b@(Div (i,cs,a) bs) | "Solution" `elem` cs = do
            modifyIORef r (. (b:))
            return []
        go b = return [b]
    bs' <- walkBlocks go bs
    sols <- readIORef r
    return $ Pandoc meta (sols [])

-- Walks in order
walkBlocks :: Monad m => (Block -> m [Block]) -> [Block] -> m [Block]
walkBlocks f = gos
  where
    gos bs = fmap concat (mapM f bs) >>= mapM go

    goss = mapM gos

    gossp (x,bss) = (x,) <$> goss bss


    go (OrderedList x bss) = OrderedList x <$> goss bss
    go (BulletList bss) = BulletList <$> goss bss
    go (DefinitionList bss) = DefinitionList <$> mapM gossp bss
    go (Div a bs) = Div a <$> gos bs
    go b = return b

