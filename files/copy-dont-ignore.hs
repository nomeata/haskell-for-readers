copyFile :: FilePath -> FilePath -> IO ()
copyFile from to = do
    content <- readFile from
    putStrLn ("Read " ++ show (length content) ++ " bytes.")
    writeFile to content

don'tignore :: IO () -> IO ()
don'tignore action = do
    putStrLn "About to execute the action."
    action
    putStrLn "I executed the action."

main :: IO ()
main = do
    putStrLn "Which file do you want to copy?"
    from <- getLine
    putStrLn "Where do you want to copy it to?"
    to <- getLine
    don'tignore (copyFile from to)
    putStrLn "Done copying."

