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

type MatchState = ([Char],[Char])

split ([],    rem) = [rem]
split ((x:xs),rem) = rem : split (xs,(x:rem))

advance :: MatchState -> [MatchState]
advance ("",         ""     ) = [("","")]
advance ("",         _      ) = []
advance (".",        [_]    ) = [("","")]
advance (".",        _      ) = []
advance ([p],        [t]    ) = if p == t then [("","")] else []
advance ([_],        _      ) = []
advance ('.':'*':xs, txt    ) = map (\r -> (xs,r)).split $ ((reverse txt),[])
advance ('.':xs,     []     ) = []
advance ('.':pxs,    (_:txs)) = [(pxs,txs)]
advance (c:'*':xs,   txt    ) = map (\r -> (xs,r)).split.span (==c) $ txt 
advance (_:_,        []     ) = []
advance (p:pxs,      t:txs  ) = if (p == t) then [(pxs,txs)] else []

match [] = False
match xs = if ("","") `elem` xs then True else match $ concatMap advance xs
