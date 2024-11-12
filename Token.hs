module Token where

data Token
  = NUM Double
  | INT Integer
  | ID String
  | ADD
  | SUB
  | MUL
  | DIV
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
  deriving (Eq, Show)
  
