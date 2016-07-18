module Euler.P040 where



champernowne = '0':concatMap show [1..]

euler :: Integer
euler = product . map (read .(:[]). (champernowne !!)) $ [1,10,100,1000,10000,100000,1000000]