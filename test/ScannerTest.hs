module Main where

import Scanner ( Token(..), TokenType(..), alexMonadScan, runAlex )

scan :: String -> [Token]
scan s = case runAlex s tokens of
  Right tokens -> tokens
  Left s -> error $ "Scanner failed " ++ s
  where
    tokens = do tok <- alexMonadScan
                case tok of Token _ TEOF -> return []
                            _            -> (:) <$> pure tok <*> tokens

printTokens :: String -> IO ()
printTokens s = do putStrLn $ "Scanning: \"" ++ s ++ "\""
                   putStrLn "Tokens:"
                   mapM_ (putStrLn . show) $ scan s

main :: IO ()
main = do putStrLn "Scanner test"
          printTokens "123"
          printTokens " 123 "
          printTokens " 123 42"
