{
module Parser where

import Token
import qualified Lex as L
import System.IO
import Types
import Aux
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

Program : Declaracoes     {$1}                                

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

-- ChamaFuncao modificado para cobrir apenas funções genéricas
ChamaFuncao: Id '(' Expr ')'            {Proc $1 [$3]}
     | Id '(' ')'                        {Proc $1 []}

TCons: Num                   {CDouble $1}
     | Int                   {CInt $1}

Bloco: '{' ListaDeCmd '}'               {$2}

Retorno: 'return' Expr ';'              {Ret (Just $2)}
     | 'return' ';'                     {Ret Nothing}

CmdSe: 'if' '(' ExprL ')' Bloco 'else' Bloco            {If $3 $5 $7} 
     | 'if' '(' ExprL ')' Bloco                          {If $3 $5 []}

CmdEnquanto: 'while' '(' ExprL ')' Bloco                 {While $3 $5}

CmdAtrib: Id '=' Expr ';'               {Atrib $1 $3}

-- Comandos específicos para 'print' e 'read'
CmdEscrita: 'print' '(' Expr ')' ';'   {Imp $3}
CmdLeitura: 'read' '(' Id ')' ';'       {Leitura $3}

-- Mantenha o ChamaProc como uma chamada genérica para funções
ChamaProc: ChamaFuncao ';'              { $1 }

Comando: CmdSe           {$1}
     | CmdEnquanto       {$1}
     | CmdAtrib          {$1}
     | CmdEscrita        {$1}
     | CmdLeitura        {$1}
     | ChamaProc         {$1}
     | Retorno           {$1}

ListaDeCmd: ListaDeCmd Comando          {$1 ++ [$2]}
     | Comando                          {[$1]}


Declaracao: Tipo ListaId ';' {sheki $1 $2}

Declaracoes: Declaracoes Declaracao          {$1 ++ [$2]}
     | Declaracao                            {[$1]}

ListaId: ListaId ',' Id            {$1 ++ [$3]}
     | Id                          {[$1]}

BlocoPrincipal: '{' Declaracoes ListaDeCmd '}'         {$3}
     | '{' ListaDeCmd '}'                              {$2}

Funcao: TipoRetorno Id '(' DeclParametros ')' BlocoPrincipal          {($2, $4, $6)}
     | TipoRetorno Id '('  ')' BlocoPrincipal                        {($2, [], $5)}

ListaDeFuncao: ListaDeFuncao Funcao               {$1 ++ [$2]}
     | Funcao                                     {[$1]}


{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do
  handle <- openFile "test.galu" ReadMode
  s <- hGetContents handle
  print (calc (L.alexScanTokens s))
  hClose handle
}
