{-
 Given a string containing just the characters '(', ')', '{', '}', 
 '[' and ']', determine if the input string is valid. The brackets 
 must close in the correct order, "()" and "()[]{}" are all valid 
 but "(]" and "([)]" are not.
-}

type Stack = String

match ')' ('(':xs) = Just xs
match '}' ('{':xs) = Just xs
match ']' ('[':xs) = Just xs
match _   _        = Nothing

step :: Char -> Stack -> Maybe Stack
step c st
  | c `elem` "({[" = Just (c:st)
  | otherwise      = match c st

calc' ""     st = Just st
calc' (x:xs) st = do 
  ns <- step x st
  calc' xs ns

calc s = calc' s "" == Just ""
