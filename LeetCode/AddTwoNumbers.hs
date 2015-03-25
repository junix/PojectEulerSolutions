{-
You are given two linked lists representing two non-negative numbers.
The digits are stored in reverse order and each of their nodes contain a single digit.
Add the two numbers and return it as a linked list.

Input: (2 -> 4 -> 3) + (5 -> 6 -> 4)
Output: 7 -> 0 -> 8
-}

import Control.Monad.State

addOne a b = state $
  \(carry,acc) -> 
     let sumd   = a + b + carry 
         ncarry = if sumd >= 10 then 1 else 0
         nacc   = sumd `mod` 10 : acc
     in  ((),(ncarry,nacc))

addAll [] [] = return ()
addAll (x1:xs1) (x2:xs2) = do 
  addOne x1  x2
  addAll xs1 xs2

reg ([],[]) = ([],[])
reg ([],x:xs) = let (l,r) = reg ([],xs) in (0:l,x:r)
reg (x:xs,[]) = let (l,r) = reg (xs,[]) in (x:l,0:r)
reg (xl:xsl,xr:xsr) = let (l,r) = reg (xsl,xsr) in (xl:l,xr:r)

addTwoNumber l r = 
  let (nl,nr)     = reg (l,r)
      stateAll    = addAll nl nr
      (carry,acc) = execState stateAll (0,[])
      res         = if carry > 0 then carry : acc else acc
  in  reverse res
