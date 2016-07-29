module Euler.P089 where

main = do
    c <- readFile "./p089_roman.txt"
    mapM_ print $ (filter (uncurry (/=)). map (\x -> (x,best x)) . lines) c

data R = I

val 'I' = 1
val 'V' = 5
val 'X' = 10
val 'L' = 50
val 'C' = 100
val 'D' = 500
val 'M' = 1000

vals :: String -> Integer
vals = sum . map val

roman 0 = []
roman n = d:roman (n - val d)
    where d = head . dropWhile ((n<).val) $ "MDCLXVI"

best :: String -> String
best = roman . vals

saved :: String -> Int
saved xs = length xs - (length . roman . vals) xs

