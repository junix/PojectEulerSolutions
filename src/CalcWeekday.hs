-- 计算星期几 
module CalcWeekday where

type Year    = Int
type Month   = Int
type Day     = Int
type Weekday = Int
data Week    = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Eq,Show,Enum)

week' :: Year -> Month -> Day -> Weekday
week' y m d = let { ny = y -1 } in 
              (ny + (ny `div` 4) - (ny `div` 100) + (ny `div` 400) + d) `mod` 7

isLeapYear :: Year -> Bool
isLeapYear x = (x `mod` 4 == 0) && (x `mod` 100 /= 0) || (x `mod` 400 == 0)

dayOfMonth :: Year -> Month -> Int
dayOfMonth y m 
  | isLeapYear y && m == 2     = 29
  | m == 2                     = 28
  | m `elem` [1,3,5,7,8,10,12] = 31
  | m `elem` [4,6,9,11]        = 30
  | otherwise                  = error $ "no a valid month:" ++ show m

week :: Year -> Month -> Day -> Week
week y m d = toEnum $ week' y m $ (sum . map (dayOfMonth y) $ (take (m-1) [1..])) + d :: Week


