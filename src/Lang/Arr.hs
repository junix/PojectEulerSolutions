module Lang.Arr where
import Control.Array

test = proc () -> do
   question <- ask -< "what is the question ?"
   answer   <- ask -< question
   returnA -< ("the answer to '" ++ question ++ "' is " ++ answer)

