{-# LANGUAGE UnicodeSyntax #-}
module GGood.Ch14.DiffList where

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList  ∷ [a] → DiffList a
toDiffList l = DiffList {getDiffList = (l++)}

toList ∷ DiffList a → [a]
toList d = getDiffList d []
