module Euler.P058 where
import Math.NumberTheory.Primes
import Data.List (partition)

select' 1 = [1]
select' n = map ((e-).(s*)) [0..3]
    where e = n*n
          s = n -1

countP n = (\(x,y) -> (length x,length y)) . partition isPrime . select' $ n

euler =  snd . head . filter isOk . go (0,1) $ 3
    where  go (p,np) n = (h, n) : go h (n+2)
              where (p',np') = countP n
                    h = (p+p',np+np')
           isOk ((p,np),l) = 10*p < np + p
