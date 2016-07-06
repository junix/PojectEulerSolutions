module GGood.Ch08.Seq where
import Control.Monad

jseq :: Monad m => [m a] -> m [a]
jseq [] = return []
jseq (x:xs) = do
    x' <- x
    xs' <- jseq xs
    return (x':xs')

jseq' :: Monad m => [m a] -> m [a]
jseq' [] = return []
jseq' (x:xs) =
    x >>= \x' ->
    jseq' xs >>= \xs' ->
    return (x':xs')

