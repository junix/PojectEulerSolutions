module Euler.P145 where

carry c = if c == 1 then (>=10) else (<10)

select :: Int          -- leftOrder
       -> Int          -- need carry
       -> Int          -- CarryOn 0 or 1
       -> Int          -- startFrom 0 or 1
       -> Int          -- how many choices
select 1 needc subc from = length $
    [x | x <- [from..9]
       , let s = x*2 + subc
       , carry needc s
       , odd s
    ]
select 2 needc subc from
    | needc /= subc = 0
    | otherwise     = cnt needc subc from

select n needc subc from = cnt needc subc from * select (n-2) subc needc 0

cnt needc subc from = length $
    [(x,y) | x <- [from..9]
           , y <- [from..9]
           , let s = x + y
           , carry needc s
           , odd (s+subc)
    ]

euler = sum [select x y 0 1 | x <- [1..8], y <- [0..1]]