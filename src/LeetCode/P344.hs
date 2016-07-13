module LeetCode.P344 where


{-
Write a function that takes a string as input and returns the string reversed.
Example:
Given s = "hello", return "olleh".

Subscribe to see which companies asked this question
-}

jfoldl :: (b->a->b) -> b -> [a] -> b
jfoldl f b [] = b
jfoldl f b (x:xs) = foldl f (f b x) xs

jreverse :: String -> String
jreverse = jfoldl (flip (:)) []