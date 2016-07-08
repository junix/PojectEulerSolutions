module Disemvowel where

disemvowel :: String -> String
disemvowel str = [c | c <- str, not (isVowel c)]
  where isVowel c = c `elem` "aeiouAEIOU"

