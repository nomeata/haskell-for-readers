{-# LANGUAGE LambdaCase #-}
import Text.Pandoc.JSON
import Data.IORef

main :: IO ()
main = do
    c <- newIORef 0
    toJSONFilter $ \case
        (Div (i,cs,a) bs) | "Exercise" `elem` cs -> do
            n <- readIORef c
            let n' = n + 1
            writeIORef c $! n'
            return $ Div (i,cs,a) (Para [Strong [Str "Exercise", Space, Str (show n')]] : bs)
        (Div (i,cs,a) bs) | "Solution" `elem` cs -> do
            n <- readIORef c
            return $ Div (i,cs,a) (Para [Strong [Str "Solution", Space, Str (show n)]] : bs)
        b -> return b
