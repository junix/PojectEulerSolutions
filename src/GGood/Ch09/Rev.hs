import Control.Monad (forever)
import Data.Char (toUpper)

main = forever $ map toUpper <$> getLine >>= putStrLn
