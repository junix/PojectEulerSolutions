import qualified Data.ByteString.Lazy.Char8 as LB
main =
    LB.getContents >>= return.sumFile >>= print
  where
    toInt   = (fst <$>). LB.readInt
    sumFile = (sum <$>). sequenceA. map toInt. LB.words

