module Token where

data Token
  = DOUBLE Double
  | INT Integer
  | ID String
  | ADD
  | SUB
  | MUL
  | DIV
  | SEMICOLON
  | COMMA
  | OCURL
  | CCURL
  | LPAR
  | RPAR
  | MORE
  | MOREEQ
  | LESS
  | LESSEQ
  | EQUAL
  | AND
  | OR
  | DIFF
  | UMINUS
  | NOT
  | TDOUBLE
  | TINT
  | TSTRING
  | TVOID
  | IF
  | ELSE
  | WHILE
  | FOR
  | ATRIB
  | LEITURA
  | PRINT
  | RETURN
  | LITERAL
  deriving (Eq, Show)