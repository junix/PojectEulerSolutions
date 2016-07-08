import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Applicative (liftA)
main = do
    contents <- LB.getContents
    print (sumFile contents)
  where sumFile = liftA sum . sequenceA. map ((liftA fst) . LB.readInt) . LB.words

