module Euler.P067 where
import System.IO

getTri = do
    content <- readFile "p067_triangle.txt"
    let tri = [map read vec :: [Integer] |vec <- (map words. lines $ content)]
    print $ euler.reverse $ tri

calc :: [Integer] -> [[Integer]] -> Integer
calc (p:_) ([c]:[]) = p+c
calc ps  (cs:css) = calc (zipWith max ms (tail ms)) css
    where ms = zipWith (+) ps cs

euler xss = calc (repeat 0) xss
