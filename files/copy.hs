main = do
    putStrLn "Which file do you want to copy?"
    from <- getLine
    putStrLn "Where do you want to copy it to?"
    to <- getLine
    content <- readFile from
    putStrLn ("Read " ++ show (length content) ++ " bytes.")
    writeFile to content
    putStrLn "Done copying."

copyFile :: FilePath -> FilePath -> IO ()
copyFile from to = do
    content <- readFile from
    putStrLn ("Read " ++ show (length content) ++ " bytes.")
    writeFile to content

main :: IO ()
main = do
    putStrLn "Which file do you want to copy?"
    from <- getLine
    putStrLn "Where do you want to copy it to?"
    to <- getLine
    copyFile from to
    putStrLn "Done copying."

return :: a -> IO a

fileSize :: FilePath -> IO Integer
fileSize path = do
  content <- readFile path
  return (length content)

