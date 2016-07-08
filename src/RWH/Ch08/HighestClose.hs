import qualified Data.ByteString.Lazy.Char8 as L
import System.Environment

main = do
    files <- System.Environment.getArgs
    if null files
        then putStrLn "no data file"
        else L.readFile (head files) >>= return . maxPrice >>= print

maxPrice = maximum . (Nothing:) . map closing . L.lines

readPrice :: L.ByteString -> Maybe Int
readPrice str =
  case L.readInt str of
    Nothing ->
        Nothing
    Just (dollars ,rest) ->
        case L.readInt (L.tail rest) of
            Nothing ->
                Nothing
            Just (cents, _) ->
                Just (dollars * 100 + cents)

readPrice' :: L.ByteString -> Maybe Int
readPrice' str = do
  (dollars, rest) <- L.readInt str
  (cents, _) <- L.readInt (L.tail rest)
  return (dollars * 100 + cents)

closing :: L.ByteString -> Maybe Int
closing = readPrice . (!!4) . L.split ','

