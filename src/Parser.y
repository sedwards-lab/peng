{

{-
happy -i Parser.y -o /dev/null

Then check Parser.info

-}

module Parser where
  
import Scanner

}

%name parse
%error { parseError }
%lexer { lexerForHappy } { Token _ TEOF }
%monad { Alex }
%tokentype { Token }

%token
  'if'    { Token _ TIf }
  'then'  { Token _ TThen }
  'else'  { Token _ TElse }
  'while' { Token _ TWhile }
  'do'    { Token _ TDo }
  'let'   { Token _ TLet }
  'case'  { Token _ TCase }
  'of'    { Token _ TOf }
  '='     { Token _ TEq }
  '+'     { Token _ TPlus }
  ';'     { Token _ TSemicolon }
  ':'     { Token _ TColon }
  '('     { Token _ TLparen }
  ')'     { Token _ TRparen }
  '{'     { Token _ TLbrace }
  '}'     { Token _ TRbrace }
  '['     { Token _ TLbracket }
  ']'     { Token _ TRbracket }
  int     { Token _ (TInteger $$) }  
  string  { Token _ (TString $$) }
  id      { Token _ (TId $$) }


%left ';'
%nonassoc NOELSE
%nonassoc 'else'

%left '+'

%%

program : topdecl              { [$1] }
        | program ';' topdecl  { $3 : $1 }

topdecl : 'let' '{' id '=' expr '}' { [] }

expr : expr ';' expr0 { [] }
     | expr0          { $1 }

expr0 : 'if' expr 'then' expr0 elseOpt { [] }
      | 'while' expr 'do' expr0 { [] }
      | 'let' '{' binds '}'     { [] }
      | expr1  { $1 }
      
elseOpt : {- nothing -} %prec NOELSE { [] }
        | ';' 'else' expr0           { [] }

expr1 : expr1 '+' expr1 { [] }
      | apply           { $1 }            

apply : apply aexpr { [] }
      | aexpr       { $1 }

aexpr : int             { [] }
      | string          { [] }
      | id              { [] }
      | '(' expr ')'    { $2 }
      | '{' expr '}'    { $2 }

binds : binds ';' bind  { $3 : $1 }
      | bind            { [$1] }

bind : id '=' aexpr     { [] }

       
{

data Lit = SLit String
         | ILit Integer
  deriving Show
  
parseError :: Token -> a
parseError t = error $ case t of
  Token (AlexPn _ l c) _ -> show l ++ ":" ++ show c ++ ":Syntax error"

}
