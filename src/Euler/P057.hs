module Euler.P057 where
import Data.Ratio
--import Control.Monad.Instances
import Control.Monad

iter :: Ratio Integer -> Ratio Integer
iter x = d % ((2*d) + n)
    where n = numerator x
          d = denominator x

seqs = go (1%2)
  where go x = (1 + x) : go (iter x)

euler = length . filter moreDigits . take 1000 $ seqs

moreDigits :: Ratio Integer -> Bool
moreDigits = do
  x <- (length.show.numerator)
  y <- (length.show.denominator)
  return (x > y)
