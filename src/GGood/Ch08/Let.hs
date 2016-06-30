import Data.Char (toUpper)

main = do { putStrLn "What's your first name?"
          ; fname <- getLine
          ; putStrLn "What's your middle name?"
          ; mname <- getLine
          ; let firstName = map toUpper fname
                middleName = map toUpper mname
          ; putStrLn $ "hey, " ++ firstName ++ "." ++ middleName}
