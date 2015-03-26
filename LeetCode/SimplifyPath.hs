{-
    Given an absolute path for a file (Unix-style), simplify it.
    For example,
    path = "/home/", => "/home"
    path = "/a/./b/../../c/", => "/c"
    Corner Cases:
    • Did you consider the case where path = "/../"? 
      In this case, you should return "/".
    • Another corner case is the path might contain 
      multiple slashes '/' together, such as "/home//foo/".
      In this case, you should ignore redundant slashes 
      and return "/home/foo".
-}
import Data.List

split :: String -> [String] -> [String]
split ""   acc = reverse acc
split "/"  acc = reverse acc
split path acc = 
  let left = dropWhile (=='/') path
      (node,nleft) = span (/='/') left
  in  if null node then reverse acc else split nleft (node:acc)

parent ["/"] = ["/"]
parent paths = init paths

norm fmt []        = fmt
norm fmt (".":xs)  = norm fmt xs
norm fmt ("..":xs) = norm (parent fmt) xs
norm fmt (x:xs)    = norm (fmt ++ [x]) xs

pathjoin :: [String] -> String
pathjoin ("/":xs)  = "/" ++ (concat.intersperse "/" $ xs)

--calc :: String -> String
calc = pathjoin. norm ["/"]. flip split []
