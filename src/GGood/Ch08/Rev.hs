main :: IO()
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ rev line
            main
    where rev = unwords . map reverse . words
