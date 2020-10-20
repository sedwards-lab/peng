{

-- https://stackoverflow.com/questions/20315739/how-to-use-an-alex-monadic-lexer-with-happy

module Scanner ( lexerForHappy, alexMonadScan, runAlex,
                 Token(..), AlexPosn(..), TokenType(..) ) where
}

%wrapper "monadUserState"

$digit = 0-9
$blank = $white # \n

tokens :-

  $blank+ ; -- Whitespace
  
  "//".*  ; -- Single-line comments
  
  $digit+ { \ (pos,_,_,s) len ->
              return $ Token pos $ TInteger $ read $ take len s }

{

data AlexUserState = AlexUserState [Int]

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState []

data Token = Token AlexPosn TokenType
  deriving Show

data TokenType =
    TEOF
  | TInteger Integer
  | TString String
  deriving Show

lexerForHappy :: (Token -> Alex a) -> Alex a
lexerForHappy = (alexMonadScan >>=)

alexEOF :: Alex Token
alexEOF = return $ Token (AlexPn 0 0 0) TEOF
}
