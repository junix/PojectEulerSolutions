module Euler.P036 where

toBinFmt 0 = []
toBinFmt 1 = [1]
toBinFmt n = r : toBinFmt d
    where (d,r) = n `quotRem` 2

toDecFmt n = show n

isPalind n = dec == reverse dec && bin == reverse bin
    where dec = show n
          bin = toBinFmt n

euler36 =  sum . filter isPalind $ [1..10^6]

