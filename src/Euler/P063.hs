module Euler.P063 where

select n = filter ((==n) . length . show) . map (^n) $ [1..9]

range = map fst . takeWhile (\(x,y) -> x == y). zip [1..] . map (length. show.(9^)) $ [1..]

euler = length . concatMap select $ range
