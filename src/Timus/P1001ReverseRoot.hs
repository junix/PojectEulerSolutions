module Main (main) where
import Text.Printf (printf)

main = do
    h <- getContents
    let xs = reverse [ read f | l <- lines h, f <- words l] :: [Integer]
        ds = map (sqrt . fromInteger)  xs :: [Double]
    mapM_ (putStrLn . (printf "%.4f") ) ds
