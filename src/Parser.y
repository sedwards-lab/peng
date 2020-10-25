{

{-
happy -i Parser.y -o /dev/null

Then check Parser.info

-}

module Parser where
  
import Scanner
import Ast

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
  'loop'  { Token _ TLoop }
  'let'   { Token _ TLet }
  'and'   { Token _ TAnd }  
  'case'  { Token _ TCase }
  'of'    { Token _ TOf }
  'after' { Token _ TAfter }
  'wait'  { Token _ TWait }
  '='     { Token _ TEq }
  '<-'    { Token _ TLarrow }
  ':'     { Token _ TColon }
  '+'     { Token _ TPlus }
  ';'     { Token _ TSemicolon }
  '|'     { Token _ TBar }
  ','     { Token _ TComma }
  '('     { Token _ TLparen }
  ')'     { Token _ TRparen }
  '{'     { Token _ TLbrace }
  '}'     { Token _ TRbrace }
  '['     { Token _ TLbracket }
  ']'     { Token _ TRbracket }
  int     { Token _ (TInteger $$) }  
  string  { Token _ (TString $$) }
  id      { Token _ (TId $$) }
  duration { Token _ (TDuration $$) }

%left ';'
%nonassoc NOELSE
%nonassoc 'else'

%left '+'

%%

program : topdecls { Program (reverse $1) }

topdecls : topdecl               { [$1] }
         | topdecls ';' topdecl  { $3 : $1 }

topdecl : id '(' optFormals ')' '=' '{' expr '}' { Function $1 $3 $7 }

optFormals : {- nothing -} { [] }
           | formals       { reverse $1 }

formals : formal             { [$1] }
        | formals ',' formal { $3 : $1 }

formal : ids ':' typs   { Bind (reverse $1) $3 }

ids : id          { [$1] }
    | ids ',' id  { $3 : $1 }

typs : typ      { $1 }
     | typs typ { TApp $1 $2 }
    
typ: id           { TCon $1 }
   | '(' typs ')' { $2 }
   


expr : expr ';' expr0 { Seq $1 $3 }
     | expr0          { $1 }

expr0 : 'if' expr 'then' expr0 elseOpt  { IfElse $2 $4 $5 }
      | 'while' expr 'do' expr0         { While $2 $4 }
      | 'let' '{' binds '}'             { Let (reverse $3) }
      | 'loop' expr0                    { Loop $2 }
      | 'after' aexpr id '<-' expr0     { After $2 $3 $5 }
      | 'wait' ids                      { Wait $2 }
      | id '<-' expr0                   { Assign $1 $3 }
      | expr1                           { $1 }
      
elseOpt : {- nothing -} %prec NOELSE { NoExpr }
        | ';' 'else' expr0           { $3 }

expr1 : expr1 '+' expr1 { BinOp $1 "+" $3 }
      | apply           { $1 }            

apply : apply aexpr { Apply $1 $2 }
      | aexpr       { $1 }

aexpr : int             { IntLit $1 }
      | string          { StringLit $1 }
      | duration        { DurLit $1 }
      | id              { Id $1 }
      | '(' expr ')'    { $2 }
      | '{' expr '}'    { $2 }

binds : binds 'and' bind  { $3 : $1 }
      | bind              { [$1] }

bind : id '=' aexpr       { Def $1 $3 }

       
{

data Lit = SLit String
         | ILit Integer
  deriving Show
  
parseError :: Token -> a
parseError t = error $ case t of
  Token (AlexPn _ l c) _ -> show l ++ ":" ++ show c ++ ":Syntax error"

}
