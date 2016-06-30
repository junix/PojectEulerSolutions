main :: IO ()
main = do
    putStrLn "What's your name?"
    let getName = getLine
    name <- getName
    putStrLn $ "Hello " ++ name ++ ", you rock!"
