module PutJson where
import SimpleJSON
import Data.List (intercalate)

renderJValue :: JValue -> String
renderJValue JNull         = "null"
renderJValue (JString s)   = show s
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue (JNumber n)   = show n
renderJValue (JArray xs)   = "[" ++ values ++ "]"
  where values = intercalate "," $ [renderJValue x | x <- xs]
renderJValue (JObject xs)  = "{" ++ values ++ "}"
  where
    pairs = [show k++": "++renderJValue v | (k,v)<- xs]
    values = intercalate "," pairs

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)

