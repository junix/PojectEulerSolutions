module Main(main) where

import Control.Monad

main = do
    ns <- getLine
    let n = read ns :: Integer
    lines <- forM [1..n] $ \_ -> getLine
    mapM_ (putStrLn.pp) lines

pp xs = concat . zipWith (\i c -> if even i then [c] else []) [0..] $ cs
    where cs = take (length xs `quot` 2) xs