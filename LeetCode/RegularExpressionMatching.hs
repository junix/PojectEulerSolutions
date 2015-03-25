{-
    Implement regular expression matching with support for '.' and '*'.

    '.' Matches any single character.
    '*' Matches zero or more of the preceding element.

    The matching should cover the entire input string (not partial).

    The function prototype should be:
    bool isMatch(const char *s, const char *p)

    Some examples:
    isMatch("aa","a") → false
    isMatch("aa","aa") → true
    isMatch("aaa","aa") → false
    isMatch("aa", "a*") → true
    isMatch("aa", ".*") → true
    isMatch("ab", ".*") → true
    isMatch("aab", "c*a*b") → true
-}

type MatchState = (String,String)

split ([],  rem) = [rem]
split (x:xs,rem) = rem : split (xs,x:rem)

step :: MatchState -> [MatchState]
step ("",         ""   ) = [("","")]
step ("",         _    ) = []
step (".",        [_]  ) = [("","")]
step (".",        _    ) = []
step ([p],        [t]  ) = [("","") | p == t]
step ([_],        _    ) = []
step ('.':'*':xs, txt  ) = map (\r -> (xs,r)).split $ (reverse txt,[])
step ('.':xs,     []   ) = []
step ('.':pxs,    _:txs) = [(pxs,txs)]
step (c:'*':xs,   txt  ) = map (\r -> (xs,r)).split.span (==c) $ txt 
step (_:_,        []   ) = []
step (p:pxs,      t:txs) = [(pxs,txs) | p == t]

match [] = False
match xs = ("","") `elem` xs || match $ concatMap step xs
