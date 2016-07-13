module Euler.P049 where
import Data.List(sort)

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

pr = takeWhile (<10000) . dropWhile (<1000) $ primes

seek [] = []
seek (x:xs) = if y `elem` xs && z `elem` xs && isPerm x y z
              then [x,y,z]:seek xs
              else seek xs
    where y = x + 3330
          z = y + 3330

isPerm x y z = let xset = cset x
                   yset = cset y
                   zset = cset z
                in xset == yset && yset == zset
    where cset = sort.show

euler49 = concat.map show.(!!1) $ seek pr