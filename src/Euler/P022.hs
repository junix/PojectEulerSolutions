module Euler.P022 where
import System.IO
import Data.List.Split
import Data.Char (ord)
import Data.List (sort)

euler = do
    c <- readFile "./p022_names.txt"
    print $calc c

w :: String -> [String]
w  = map read. wordsBy (==',')

worth xs = sum . map (subtract start). map ord $ xs
    where start = ord 'A' - 1

calc = sum . zipWith (*) [1..] . map worth. sort . w

