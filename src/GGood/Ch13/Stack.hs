{-# LANGUAGE UnicodeSyntax #-}
import Control.Monad.State

type Stack = [Int]

push ∷ Int → State Stack ()
push x = state $ \xs -> ((),x:xs)

pop ∷ State Stack Int
pop = state $ (\(x:xs) -> (x, xs))
