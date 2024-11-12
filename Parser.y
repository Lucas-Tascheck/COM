{
module Parser where

import Token
import qualified Lex as L
import System.IO
import Types

}

%name calc
%tokentype { Token }
%error { parseError }
%token 
  '+' {ADD}
  '-' {SUB}
  '*' {MUL}    
  '/' {DIV}
  '(' {LPAR}
  ')' {RPAR}
  '>' {MORE}
  '>=' {MOREEQ}
  '<' {LESS}
  '<=' {LESSEQ}
  '!=' {DIFF}
  '==' {EQUAL}
  '&&' {AND}
  '||' {OR}
  '!' {NOT}
  ';' {SEMICOLON}
  ',' {COMMA}
  '{' {OCURL}
  '}' {CCURL}
  'int' {TINT}
  'double' {TDOUBLE}
  'void' {TVOID}
  'string' {TSTRING}  
  Num {NUM $$}
  Id {ID $$}
  Int {INT $$}

%left '||'
%left '&&'
%nonassoc '==' '!=' '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%right '!'
%right UMINUS

%%

Program : ExprL             { Logic $1 }
      | Expr                { Arit $1 }

Expr  : Expr '+' Expr       { $1 :+: $3 }
      | Expr '-' Expr       { $1 :-: $3 }
      | Expr '*' Expr       { $1 :*: $3 }
      | Expr '/' Expr       { $1 :/: $3 }
      | '(' Expr ')'        { $2 }
      | '-' Expr %prec UMINUS { Neg $2 }
      | TCons                 { Const $1 }  
      | Id                  {IdVar $1}

ExprR : Expr '>' Expr       { $1 :>: $3 }
      | Expr '<' Expr       { $1 :<: $3 }
      | Expr '>=' Expr      { $1 :>=: $3 }
      | Expr '<=' Expr      { $1 :<=: $3 }
      | Expr '==' Expr      { $1 :==: $3 }
      | Expr '!=' Expr      { $1 :/=: $3 }

ExprL : ExprL '||' ExprL    { $1 :|: $3 }
      | ExprL '&&' ExprL    { $1 :&: $3 }
      | '!' ExprL           { Not $2 }
      | ExprR               {Rel $1}

Tipo: 'int'                  { TInt } 
     | 'double'              { TDouble }
     | 'string'              { TString }
     | 'void'                { TVoid }

TCons: Num                   {CDouble $1}
     | Int                   {CInt $1}

Var: Tipo Id ';'                 {$2 :#: $1}

Funcao: Id '(' 

{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do
  handle <- openFile "test.galu" ReadMode
  s <- hGetContents handle
  print (calc (L.alexScanTokens s))
  hClose handle
}
