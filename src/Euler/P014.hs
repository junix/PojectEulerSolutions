module Euler.P014 where
import Data.Bits (shiftR, (.&.))
import Control.Monad.State
import Control.Parallel.Strategies
import Data.Tuple (swap)
import Data.List (maximumBy)
import qualified Data.IntMap.Strict as M
{-
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
-}

lenOfChain :: Int -> Int
lenOfChain 1 = 1
lenOfChain n
    | n .&. 1 == 0 = 1 + lenOfChain (n `quot` 2)
    | otherwise    = 1 + lenOfChain (3 * n + 1)

chain :: Int -> [Int]
chain 1 = [1]
chain n
    | n .&. 1 == 0 = n : chain (n `quot` 2)
    | otherwise    = n : chain (3 * n + 1)

next :: Int -> Int
next 1 = 1
next n
    | n .&. 1 == 0 = n `quot` 2
    | otherwise    = 3 * n + 1

type S = M.IntMap Int

calc :: Int -> State S Int
calc 1 = state $ \s -> (1, M.insert 1 1 s)
calc n = state $ \s ->
    case M.lookup n s of
       Just l  -> (l, s)
       Nothing -> let (l', s') = runState (calc (next n)) s
                      l = 1 + l'
                  in  l `seq` (l, M.insert n l s')

calcRange :: Int -> Int -> State S ()
calcRange from end
    | from == end = return ()
    | otherwise   = calc from >> calcRange (from+1) end

solute :: Int -> Int -> Int
solute f e = snd . maximum . map swap . M.toList . snd $ runState (calcRange f e) M.empty

{-
parSol :: Int
parSol = runEval $ do
    n1 <- rpar (solute 1      250000)
    n2 <- rpar (solute 250000 500000)
    n3 <- rpar (solute 500000 750000)
    n4 <- rpar (solute 750000 1000000)
    return (maximum [n1,n2,n3,n4])

main = print $ solute 1 1000000
-}

