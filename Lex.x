{
module Lex where

import Token
}

%wrapper "basic"

$digit = [0-9]      
$letter = [:a-zA-Z ]   

@double = $digit+(\.$digit+)? 
@int = $digit+ 
@id = $letter($letter|$digit)* 

tokens :-

<0> $white+ ;
<0> @int {\s -> INT (read s)}
<0> @double {\s -> DOUBLE (read s)}
<0> "+" {\s -> TADD}  
<0> "-" {\s -> TSUB}  
<0> "*" {\s -> TMUL}  
<0> "/" {\s -> TDIV}  
<0> "(" {\s -> TLPAR}  
<0> ")" {\s -> TRPAR}  
<0> ">" {\s -> TMORE}
<0> "<" {\s -> TLESS}
<0> ">=" {\s -> TMOREEQ}
<0> "<=" {\s -> TLESSEQ}
<0> "!=" {\s -> TDIFF}
<0> "==" {\s -> TEQUAL}
<0> "&&" {\s -> TAND}
<0> "||" {\s -> TOR}
<0> "!" {\s -> TNOT}
<0> "int" {\s -> TINT}
<0> "double" {\s -> TDOUBLE}
<0> "float" {\s -> TFLOAT}
<0> "string" {\s -> TSTRING}
<0> "void" {\s -> TVOID}
<0> """    {\s -> TLITERAL}
<0> ";" {\s -> TSEMICOLON}
<0> "," {\s -> TCOMMA}
<0> "{" {\s -> TOCURL}
<0> "}" {\s -> TCCURL}
<0> "if" {\s -> TIF}
<0> "else" {\s -> TELSE}
<0> "while" {\s -> TWHILE}
<0> "for" {\s -> TFOR}
<0> "=" {\s -> TATRIB}
<0> "read" {\s -> TLEITURA}
<0> "print" {\s -> TPRINT}
<0> "return" {\s -> TRETURN}

<0> @id  {\s -> ID s}   
{

testLex = do s <- getLine
             print (alexScanTokens s)
}
