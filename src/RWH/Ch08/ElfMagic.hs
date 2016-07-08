import qualified Data.ByteString.Lazy as L
import qualified System.IO
import qualified System.Environment

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO ()
isElfFile path = do
    content <- L.readFile path
    print $ hasElfMagic content

main :: IO ()
main = do
  paths <- System.Environment.getArgs
  mapM isElfFile paths
  return ()
