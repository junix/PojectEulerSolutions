{-# OPTIONS_GHC -Wall #-}
module Lang.Ctors where
data List a = Nil | a :-: List a

