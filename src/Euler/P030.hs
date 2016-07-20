module Euler.P030 where


isPowered n = (==n). sum . map (^5) . digits $ n
    where digits 0 = []
          digits n = r : digits q
            where (q,r) = n `quotRem` 10

poweredSeq = sum . take 6 . filter isPowered $ [10..]