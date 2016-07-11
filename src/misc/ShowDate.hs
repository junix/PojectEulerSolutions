module ShowDate where

showDay :: Int -> String
showDay day
    | r == 1 = show day ++ "st"
    | r == 2 = show day ++ "nd"
    | r == 3 = show day ++ "rd"
    | otherwise = show day ++ "th"
    where r = day `rem` 10

showMonth :: Int -> String
showMonth 1 = "January"
showMonth 2 = "February"
showMonth 3 = "March"
showMonth 4 = "April"
showMonth 5 = "May"
showMonth 6 = "June"
showMonth 7 = "July"
showMonth 8 = "August"
showMonth 9 = "September"
showMonth 10 = "October"
showMonth 11 = "November"
showMonth 12 = "Decembe"

showDate (day, month, year) = showDay day ++ " " ++ showMonth month ++ ", " ++ show year

