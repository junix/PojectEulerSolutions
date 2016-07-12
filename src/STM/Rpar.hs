-- module STM.Rpar where

import Control.Parallel.Strategies
import System.Environment

fib 1 = 1
fib 2 = 2
fib n = runEval $ do
    a <- rpar (fib (n-1))
    b <- rseq (fib (n-2))
    return (a+b)


main = do
    (n:_) <- getArgs
    print $ fib.read $ n
