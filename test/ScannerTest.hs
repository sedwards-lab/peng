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
printTokens s = do putStrLn "Input:"
                   putStrLn s
                   putStrLn "Tokens:"
                   mapM_ (putStrLn . show) $ scan s

main :: IO ()
main = do putStrLn "Scanner test"
          printTokens "123"
          printTokens " 123 "
          printTokens " 123 42"
          printTokens $ unlines ["let a = 42 // enter two blocks"
                                ,"    b = 31 // 31 a line by itself"
                                ,"        245 42 // 245 on next line"
                                ,"         contin // (line continued)"
                                ,"another 42"
                                ,"  lineContinued 82"
                                ,"while 1 2 3"
                                ," block 3"
                                ,"   4 3"
                                ," line 4"
                                ,"nothing"]
          printTokens $ "let a = let b = let c = 42"
          printTokens $ "let a = let b = let c = 42\n"
          printTokens $ "let a = let b = let c = 42\n\n"
          printTokens $ unlines ["let a = let b = let c = 42"
                                ,""
                                ,"//   "
                                ," "]
          printTokens $ unlines ["if a equals b"
                                ,"  42 57"
                                ,"    continued"
                                ,"  newline"
                                ,"else"
                                ,"  forgetit"]
          printTokens $ unlines ["if (a 42"
                                ,"     43 44"
                                ,"    cont"
                                ,"       )"
                                ,"  body1 body2"
                                ,"    cont"
                                ,"  body3"]
          printTokens "if ()"
          printTokens $ unlines ["if ()"]
          printTokens $ unlines ["if ()"
                                ," line1"]
          printTokens $ unlines ["if ()"
                                ,""
                                ," line1"
                                ,""
                                ," // Testing empty lines"
                                ,""
                                ,"  line1cont"
                                ," line2"
                                ,""
                                ,"// More empties"
                                ,""
                                ," line3"]
          printTokens $ unlines ["let a = (42"
                                ," 12 46 823"
                                ,")"
                                ,"    b = 51"
                                ,"        run"
                                ,"          jump"]
            


                                
            
