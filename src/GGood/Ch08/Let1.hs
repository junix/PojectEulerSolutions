import Data.Char (toUpper)

main =
    putStrLn "What's your first name?" >>
        getLine >>= \fname ->
            putStrLn "What's your middle name?" >>
                getLine >>= \mname ->
                    let firstName = map toUpper fname
                        middleName = map toUpper mname
                    in
                        putStrLn $ "hey, " ++ firstName ++ "." ++ middleName
