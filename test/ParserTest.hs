module Main where

import Scanner
import Parser

parseStr :: String -> Either String [[Int]]
parseStr s = runAlex s parse

main :: IO ()
main = do putStrLn "Parser test"
          print $ parseStr "42 82"
