{-
 Given a string s and a dictionary of words dict, 
 determine if s can be segmented into a space-separated 
 sequence of one or more dictionary words.

 For example, given
 s = "leetcode",
 dict = ["leet", "code"].

 Return true because "leetcode" can be segmented as 
 "leet code".
-}

import qualified Data.Set as S

wordBreak chs = wordBreak' ""  chs . S.fromList . map reverse

wordBreak' ""  "" dict     = True
wordBreak' acc "" dict     = S.member acc dict
wordBreak' acc (x:xs) dict = S.member (x:acc) dict && wordBreak' "" xs dict || 
                             wordBreak' (x:acc) xs dict
