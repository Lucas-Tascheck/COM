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
  'if' {IF}
  'else' {ELSE}
  'while' {WHILE}
  '=' {ATRIB}
  'read' {LEITURA}
  'print' {PRINT}
  'return' {RETURN}

%left '||'
%left '&&'
%nonassoc '==' '!=' '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%right '!'
%right UMINUS

%%

Program : DeclParametros           {$1}

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

TipoRetorno: Tipo            {$1}
     | 'void'                { TVoid }

Parametro: Tipo Id                 {$2 :#: $1}

DeclParametros: DeclParametros ',' Parametro                {$1 ++ [$3]}
     | Parametro                                            {[$1]}

TCons: Num                   {CDouble $1}
     | Int                   {CInt $1}




{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do
  handle <- openFile "test.galu" ReadMode
  s <- hGetContents handle
  print (calc (L.alexScanTokens s))
  hClose handle
}
