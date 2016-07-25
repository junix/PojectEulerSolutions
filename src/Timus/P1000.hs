module Main(main) where

main = do
    line <-  getLine
    let [a,b] =  words line
        x = read a :: Integer
        y = read b :: Integer
    print (x+y)

