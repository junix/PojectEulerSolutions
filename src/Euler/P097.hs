module Euler.P097 where

tenDigits n = r : tenDigits ((if n < 10^10 then n else r)*2)
    where r = n `rem` (10^10)


euler = (`rem`(10^10)) . (+1) . (28433*) . (!!7830456) . tenDigits $ 2

