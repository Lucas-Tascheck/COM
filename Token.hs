module Token where

data Token
  = DOUBLE Double
  | INT Integer
  | ID String
  | TADD
  | TSUB
  | TMUL
  | TDIV
  | TSEMICOLON
  | TCOMMA
  | TOCURL
  | TCCURL
  | TLPAR
  | TRPAR
  | TMORE
  | TMOREEQ
  | TLESS
  | TLESSEQ
  | TEQUAL
  | TAND
  | TOR
  | TDIFF
  | TUMINUS
  | TNOT
  | TDOUBLE
  | TINT
  | TSTRING
  | TVOID
  | TFLOAT
  | TIF
  | TELSE
  | TWHILE
  | TFOR
  | TATRIB
  | TLEITURA
  | TPRINT
  | TRETURN
  | TLITERAL
  deriving (Eq, Show)