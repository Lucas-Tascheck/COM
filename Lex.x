{
module Lex where

import Token
}

%wrapper "basic"

$digit = [0-9]      
$letter = [a-zA-Z]   

@num = $digit+(\.$digit+)? 
@int = $digit+ 
@id = $letter($letter|$digit)* 

tokens :-

<0> $white+ ;
<0> @num {\s -> NUM (read s)}
<0> @int {\s -> INT (read s)}
<0> @id  {\s -> ID s}   
<0> "+" {\s -> ADD}  
<0> "-" {\s -> SUB}  
<0> "*" {\s -> MUL}  
<0> "/" {\s -> DIV}  
<0> "(" {\s -> LPAR}  
<0> ")" {\s -> RPAR}  
<0> ">" {\s -> MORE}
<0> ">=" {\s -> MOREEQ}
<0> "<" {\s -> LESS}
<0> "<=" {\s -> LESSEQ}
<0> "!=" {\s -> DIFF}
<0> "==" {\s -> EQUAL}
<0> "&&" {\s -> AND}
<0> "||" {\s -> OR}
<0> "!" {\s -> NOT}
<0> "int" {\s -> TINT}
<0> "double" {\s -> TDOUBLE}
<0> "string" {\s -> TSTRING}
<0> "void" {\s -> TVOID}
{

testLex = do s <- getLine
             print (alexScanTokens s)
}
