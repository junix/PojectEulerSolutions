module WeIrDStRiNgCaSe where
import Data.Char(toUpper, toLower)

toWeirdCase :: String -> String
toWeirdCase = zipWith (\f x -> f x) (cycle [toUpper, toLower])