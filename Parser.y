{
module Parser where

import Token
import qualified Lex as L
import System.IO
import Types
import Aux
import Semantica
}

%name calc
%tokentype { Token }
%error { parseError }
%token 
  '"' {TLITERAL}
  '+' {TADD}
  '-' {TSUB}
  '*' {TMUL}    
  '/' {TDIV}
  '(' {TLPAR}
  ')' {TRPAR}
  '>' {TMORE}
  '>=' {TMOREEQ}
  '<' {TLESS}
  '<=' {TLESSEQ}
  '!=' {TDIFF}
  '==' {TEQUAL}
  '&&' {TAND}
  '||' {TOR}
  '!' {TNOT}
  ';' {TSEMICOLON}
  ',' {TCOMMA}
  '{' {TOCURL}
  '}' {TCCURL}
  'int' {TINT}
  'double' {TDOUBLE}
  'float' {TFLOAT}
  'void' {TVOID}
  'string' {TSTRING}  
  Double {DOUBLE $$}
  Float {DOUBLE $$}
  Id {ID $$}
  Int {INT $$}
  'if' {TIF}
  'else' {TELSE}
  'while' {TWHILE}
  'for' {TFOR}
  '=' {TATRIB}
  'read' {TLEITURA}
  'print' {TPRINT}
  'return' {TRETURN}

%left '||'
%left '&&'
%nonassoc '==' '!=' '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%right '!'
%right UMINUS

%%

Program : ListaDeFuncaoS BlocoPrincipal           {printSemantica (Prog (fst $1) (snd $1) (fst $2) (snd $2))}       
     | BlocoPrincipal                             {printSemantica (Prog [] [] (fst $1) (snd $1))}                        

Expr  : Expr '+' Expr                             { Add $1 $3 }
      | Expr '-' Expr                             { Sub $1 $3 }
      | Expr '*' Expr                             { Mul $1 $3 }
      | Expr '/' Expr                             { Div $1 $3 }
      | '(' Expr ')'                              { $2 }
      | '-' Expr %prec UMINUS                     { Neg $2 }
      | TCons                                     { Const $1 }  
      | Id                                        {IdVar $1}
      | Id '(' ListaDeParametros ')'              {Chamada $1 $3}
      | Id '(' ')'                                {Chamada $1 []} 
      | Literal                                   {$1}

ExprR : Expr '>' Expr       { Rgt $1 $3 }
      | Expr '<' Expr       { Rlt $1 $3 }
      | Expr '>=' Expr      { Rge $1 $3 }
      | Expr '<=' Expr      { Rle $1 $3 }
      | Expr '==' Expr      { Req $1 $3 }
      | Expr '!=' Expr      { Rdif $1 $3 }

ExprL : ExprL '||' ExprL    { Or $1 $3 }
      | ExprL '&&' ExprL    { And $1 $3 }
      | '!' ExprL           { Not $2 }
      | ExprR               {Rel $1}

ListaDeLiteral: ListaDeLiteral Id           {$1 ++ " " ++ $2}
     | ListaDeLiteral Int                   {$1 ++ " " ++ (show $2)}
     | ListaDeLiteral Double                {$1 ++ " " ++ (show $2)}
     | Id                                   {$1}
     | Int                                  {(show $1)}
     | Double                               {(show $1)}

Literal: '"' ListaDeLiteral '"'           {Lit $2}
     | '"' '"'                            {Lit []}

Tipo: 'int'                  { TInt } 
     | 'double'              { TDouble }
     | 'float'               { TFloat }
     | 'string'              { TString }

TipoRetorno: Tipo            {$1}
     | 'void'                { TVoid }

Parametro: Tipo Id                 {$2 :#: $1}

DeclParametros: DeclParametros ',' Parametro                {$1 ++ [$3]}
     | Parametro                                            {[$1]}

ListaDeParametros: ListaDeParametros ',' Expr               {$1 ++ [$3]}
     | Expr                                                 {[$1]}

ChamaFuncao: Id '(' ListaDeParametros ')'                   {Proc $1 $3}
     | Id '(' ')'                                           {Proc $1 []}

TCons: Double                   {CDouble $1}
     | Int                      {CInt $1}
     | Float                    {CFloat $1}

Bloco: '{' ListaDeCmd '}'               {$2}

Retorno: 'return' Expr ';'              {Ret (Just $2)}
     | 'return' ';'                     {Ret Nothing}

CmdSe: 'if' '(' ExprL ')' Bloco 'else' Bloco            {If $3 $5 $7} 
     | 'if' '(' ExprL ')' Bloco                          {If $3 $5 []}

CmdEnquanto: 'while' '(' ExprL ')' Bloco                 {While $3 $5}

CmdFor: 'for' '(' Id '=' Expr ';' ExprL ';' Id '=' Expr ')' Bloco  {For ($3, $5) $7 ($9, $11) $13}
     | 'for' '(' 'int' Id '=' Expr ';' ExprL ';' Id '=' Expr ')' Bloco  {For ($4, $6) $8 ($10, $12) $14}

CmdAtrib: Id '=' Expr ';'               {Atrib $1 $3}

CmdEscrita: 'print' '(' Expr ')' ';'   {Imp $3}
CmdLeitura: 'read' '(' Id ')' ';'       {Leitura $3}

ChamaProc: ChamaFuncao ';'              { $1 }

Comando: CmdSe           {$1}
     | CmdEnquanto       {$1}
     | CmdFor            {$1}
     | CmdAtrib          {$1}
     | CmdEscrita        {$1}
     | CmdLeitura        {$1}
     | ChamaProc         {$1}
     | Retorno           {$1}

ListaDeCmd: ListaDeCmd Comando          {$1 ++ [$2]}
     | Comando                          {[$1]}


Declaracao: Tipo ListaId ';' {juntaTipo $1 $2}

Declaracoes: Declaracoes Declaracao          {$1 ++ $2}
     | Declaracao                            {$1}

ListaId: ListaId ',' Id            {$1 ++ [$3]}
     | Id                          {[$1]}

BlocoPrincipal: '{' Declaracoes ListaDeCmd '}'         {($2, $3)}
     | '{' ListaDeCmd '}'                              {([], $2)}

Funcao: TipoRetorno Id '(' DeclParametros ')' BlocoPrincipal          {($2 :->: ($4,$1), ($2, fst($6), snd($6)))}
     | TipoRetorno Id '('  ')' BlocoPrincipal                        {($2 :->: ([],$1), ($2, fst($5), snd($5)))}

ListaDeFuncao: ListaDeFuncao Funcao               {$1 ++ [$2]}
     | Funcao                                     {[$1]}

ListaDeFuncaoS: ListaDeFuncao                     {separaLados $1}

{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do
  handle <- openFile "codigo.txt" ReadMode
  s <- hGetContents handle
  (calc (L.alexScanTokens s))
  hClose handle
}
