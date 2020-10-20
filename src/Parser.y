{

module Parser where
  
import Scanner

}

%name parse
%error { parseError }
%lexer { lexerForHappy } { Token _ TEOF }
%monad { Alex }
%tokentype { Token }

%token
  int    { Token _ (TInteger $$) }
  string { Token _ (TString $$)  }

%%

program : {- nothing -}     { [] }
        | lit program       { $1 : $2 }

lit : string { SLit $1 }
    | int    { ILit $1 }

{

data Lit = SLit String
         | ILit Integer
  deriving Show
  
parseError :: Token -> a
parseError t = error $ case t of
  Token (AlexPn _ l c) _ -> show l ++ ":" ++ show c ++ ":Syntax error"

}
