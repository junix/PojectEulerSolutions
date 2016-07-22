module Euler.P059 where
import Data.Bits (xor)
import Data.Char (ord,chr,isAlpha)
import Data.List.Split (wordsBy)

keySeq = [[a,b,c] | a <- chars, b <- chars, c <- chars]
    where chars = map ord ['a'..'z']

parseEncry :: String -> [Int]
parseEncry = map read . wordsBy isDelim
    where isDelim c = c == ',' || c == '\n'

decode encry passwd = map chr . zipWith xor encry . cycle $ passwd

isText :: String -> Bool
isText s  = "the" `elem` w && "with" `elem` w
    where w = words s

main = do
    contents <- readFile "./p059_cipher.txt"
    let de = decode (parseEncry contents)
    print (sum . map ord . head . filter isText . map de $ keySeq)
