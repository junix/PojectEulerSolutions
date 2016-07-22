module Euler.P055 where

{-
我们将47与它的逆转相加，47 + 74 = 121, 可以得到一个回文。
并不是所有数都能这么快产生回文，例如：

349 + 943 = 1292,
1292 + 2921 = 4213
4213 + 3124 = 7337

也就是说349需要三次迭代才能产生一个回文。

虽然还没有被证明，人们认为一些数字永远不会产生回文，例如196。那些永远不能通过上面的方法（逆转然后相加）产生回文的数字叫做Lychrel数。因为这些数字的理论本质，同时也为了这道题，我们认为一个数如果不能被证明的不是Lychrel数的话，那么它就是Lychre数。此外，对于每个一万以下的数字，你还有以下已知条件：这个数如果不能在50次迭代以内得到一个回文，那么就算用尽现有的所有运算能力也永远不会得到。10677是第一个需要50次以上迭代得到回文的数，它可以通过53次迭代得到一个28位的回文：4668731596684224866951378664。

令人惊奇的是，有一些回文数本身也是Lychrel数，第一个例子是4994。

10000以下一共有多少个Lychrel数？
-}

isPalind n = n == rev n

rev :: Integer -> Integer
rev = read . reverse . show

iter n = next : iter next
    where next = n + rev n

lychrel :: Integer -> Bool
lychrel = null . filter isPalind . take 50 . iter

euler = length . filter lychrel $ [1..9999]