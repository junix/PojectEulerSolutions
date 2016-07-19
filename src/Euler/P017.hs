module Euler.P017 where

toEn 1="one"
toEn 2="two"
toEn 3="three"
toEn 4="four"
toEn 5="five"
toEn 6="six"
toEn 7="seven"
toEn 8="eight"
toEn 9="nine"
toEn 10="ten"

toEn 11="eleven"
toEn 12="twelve"
toEn 13="thirteen"
toEn 14="fourteen"
toEn 15="fifteen"
toEn 16="sixteen"
toEn 17="seventeen"
toEn 18="eighteen"
toEn 19="nineteen"

toEn 20="twenty"
toEn 30="thirty"
toEn 40="forty"
toEn 50="fifty"
toEn 60="sixty"
toEn 70="seventy"
toEn 80="eighty"
toEn 90="ninety"

toEn 1000="one thousand"
toEn n
    | d0 /= 0 && r0 == 0 = toEn d0    ++ " hundred"
    | d0 /= 0 && r0 /= 0 = toEn d0    ++ " hundred and " ++ toEn r0
    | r0 > 19            = toEn (d1*10) ++ " " ++ toEn r1
    | otherwise          = toEn r0
    where (d0,r0) = n  `quotRem` 100
          (d1,r1) = r0 `quotRem` 10

len :: Int -> Int
len = length . filter (/=' ').toEn

euler = sum . map len $ [1..1000]




