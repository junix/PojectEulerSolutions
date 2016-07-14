module Euler.P045 where

t= map (\n->n*(n+1) `quot` 2) [1..]
n= map (\n->n*(3*n-1) `quot` 2) [1..]
h= map (\n->n*(2*n-1)) [1..]

seek ts'@(t:ts) ns'@(n:ns) hs'@(h:hs)
    | t == n && n == h = t:seek ts ns hs
    | t == m           = seek ts  ns' hs'
    | n == m           = seek ts' ns  hs'
    | otherwise        = seek ts' ns' hs
    where m = minimum [t,n,h]

euler45 = take 3 $ seek t n h
