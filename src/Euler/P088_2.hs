module Euler.P088_2 where
import Data.List
import qualified Data.Set as S

factors  = factors' [2..]
factors' (x:xs) n | n < x*x = [[n]]
                  | mod n x == 0 = (map (x:).factors' (x:xs))(div n x) ++ factors' xs n
                  | mod n x /= 0 = factors' xs n

proSumToK n = map (\ x->n-sum x+genericLength x).factors$n

update k (s,ns,n) = foldl insert' (s,ns,n+1) .filter need .proSumToK $n
    where need x = 1<=x && x<=k && S.notMember x s
          insert'(t,ms,m) k = (S.insert k t,add n ms,m)
          add x (y:ys) | x==y = y:ys
                       | x/=y = x:y:ys

p088 k = second.until allIn (update k)$(S.empty,[0],0)
    where first (a,b,c) = a
          second (a,b,c) = b
          allIn =(==k).S.size.first
main = p088$12000

