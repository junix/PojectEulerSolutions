module Main (
            main
            ) where

main = do
    a <- getLine
    b <- getLine
    let x = read a :: Integer
        y = read b :: Integer
    print (x+y)

