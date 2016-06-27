{-# LANGUAGE GADTs, GADTSyntax #-}
module Lang.GADTs where

data List a where
    Empty :: List a
    Cons :: a -> List a -> List a

