{-#LANGUAGE UnicodeSyntax#-}

module Prettify where
import SimpleJSON
import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
          deriving (Show,Eq)

fsep ∷ [Doc] → Doc
fsep = fold (</>)

(</>) ∷ Doc → Doc → Doc
x </> y = x <> softline <> y

softline ∷ Doc
softline = group line

group ∷ Doc → Doc
group x = flatten x `Union` x

flatten ∷ Doc → Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

punctuate ∷ Doc → [Doc] → [Doc]
punctuate _ []  = []
punctuate p [d] = [d]
punctuate p (x:xs) = (x <> p) : punctuate p xs

enclose ∷ Char → Char → Doc → Doc
enclose b e d = char b <> d <> char e

series ∷ Char → Char → (a → Doc) → [a] → Doc
series open close f = enclose open close . fsep . punctuate (char ','). map f

char ∷ Char → Doc 
char c = Char c

hexEscape ∷ Char → Doc
hexEscape c
    | d < 0x10000 = smallHex d
    | otherwise   = astral (d - 0x10000)
  where d = ord c

oneChar ∷ Char → Doc
oneChar c = case lookup c simpleEscape of
                Just s -> text s
                Nothing | mustEscape c -> hexEscape c
                        | otherwise    -> char c
  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'


simpleEscape ∷ [(Char, String)] 
simpleEscape = [(c, escapeChar c) | c <- "\n\r\f\t\"\\/"]

string ∷  String → Doc
string = enclose '"' '"' . hcat . map oneChar
 
empty ∷ Doc
empty = Empty

text ∷ String → Doc
text ""  = Empty
text str = Text str

double :: Double -> Doc
double d = text $ show d

line ∷ Doc
line = Line

hcat ∷ [Doc] → Doc
hcat = fold (<>)

fold ∷ (Doc → Doc → Doc) → [Doc] → Doc
fold f = foldr f empty

escapeChar ∷ Char → String
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\f' = "\\f"
escapeChar '\t' = "\\t"
escapeChar '\"' = "\\\""
escapeChar '\\' = "\\\\"
escapeChar '/' = "\\/"

(<>) ∷ Doc → Doc → Doc
Empty <> b = b
b <> Empty = b
a <> b = a `Concat` b

smallHex :: Int -> Doc
smallHex c = text "\\u"
           <> text (replicate (4-length h) '0')
           <> text h
   where h = showHex c ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
                case d of
                    Empty        -> transform ds
                    Char c       -> c : transform ds
                    Text s       -> s ++ transform ds
                    Line         -> '\n' : transform ds
                    a `Concat` b -> transform (a:b:ds)
                    _ `Union` b  -> transform (b:ds)

pretty ∷ Int → Doc → String
pretty width x = best 0 [x]
    where best col (d:ds) = 
            case d of
              Empty        -> best col ds
              Char c       -> c :  best (col + 1) ds
              Text s       -> s ++ best (col + length s) ds
              Line         -> '\n' : best 0 ds
              a `Concat` b -> best col (a:b:ds)
              a `Union` b  -> nicest col (best col (a:ds))
                                       (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
             where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs
