module Euler.P135 where
import qualified  Data.Map as M

dseq d
    | d < 500   = map (f d)  [1..3*d-1]
    | otherwise = filter (<10^6) . map (f d) $ ([1..x0] ++ [x1..3*d-1])
    where f d x = 3*d^2 + 2*d*x - x^2
          m     = 2 * (truncate . sqrt . fromInteger) (d*d-250000)
          x0    = d - m
          x1    = d + m

euler = length . map fst      .
        filter ((==10) . snd) .
        M.toList              .
        foldl (\d k -> M.insertWith (+) k 1 d) M.empty .
        concat . takeWhile (not.null) . map dseq $ [1..]
