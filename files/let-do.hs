main :: IO ()
main = do
    putStrLn "Please press enter."
    input1 <- getLine
    putStrLn "Enter pressed."

    putStrLn "Please press enter."
    let input2 = getLine
    putStrLn "Enter pressed."

