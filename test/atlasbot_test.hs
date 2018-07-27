module Main
  where

import Data.List


removePunc :: String -> String
removePunc xs = filter (not . (`elem` ",.?!-:;\"\'")) xs


splitStr :: String -> [String]
splitStr s = filter (\w -> w `notElem` [" "]) (words s)


--cleanStr :: String -> String
--cleanStr s = drop 1 . dropWhile (\= ":") s


main = do
    let p = "Test: Hello World!"
    putStrLn p
