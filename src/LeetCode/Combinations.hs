{-
    Given two integers n and k, return all possible combinations of k numbers out of 1 ... n.
    For example, If n = 4 and k = 2, a solution is:

    [
        [2,4],
        [3,4],
        [2,3],
        [1,2],
        [1,3],
        [1,4],
    ]
-}

combinations  0 _ = []
combinations  _ 0 = []
combinations  n 1 = [[x]|x<-[1..n]]
combinations  n k
  | n < k     = []
  | n == k    = [[1..n]]
  | otherwise =  combinations (n-1) k ++ map (n:) (combinations (n-1) (k-1))
