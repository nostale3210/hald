module Main.Util where

removeString :: String -> String -> String
removeString mtch str =
  case str of
    [] -> []
    [x] ->
      ([x | [x] /= mtch])
    x : xs ->
      if take ln str == mtch
        then removeString mtch (drop ln str)
        else x : removeString mtch xs
  where
    ln = length mtch

newIdentifier :: [Int] -> Int
newIdentifier = succ . maximum
