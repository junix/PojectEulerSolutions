import qualified Data.ByteString.Lazy.Char8 as LB
main = do
    contents <- LB.getContents
    print $ sumFile contents
  where
    toInt   = (fst <$>). LB.readInt
    sumFile = (sum <$>). sequenceA. map toInt. LB.words

